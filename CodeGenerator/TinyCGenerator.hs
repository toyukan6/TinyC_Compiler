module CodeGenerator.TinyCGenerator where

import qualified Data.Map as Map

import CompileError
import Syntax.AST
import Syntax.Type
import Syntax.Semantic
import Syntax.Generator

makeBinaryOperation :: (String -> String -> BinaryOperation) -> String
                                                             -> String
                                                             -> [Code]
makeBinaryOperation op e1 e2 = [BO . op e1 $ e2]

add = makeBinaryOperation GAdd
sub = makeBinaryOperation GSub
imul = makeBinaryOperation GMul
cmp = makeBinaryOperation GCmp
mov = makeBinaryOperation GMov
movzx = makeBinaryOperation GMovzx

makeMonadicOperation :: (String -> MonadicOperation) -> String
                                                     -> [Code]
makeMonadicOperation op e = [MO . op $ e]

idiv = (++) cdq . makeMonadicOperation GDiv . (++) "dword "
imod e =
    (++) cdq . (++) (makeMonadicOperation GMod . (++) "dword " $ e) . mov "eax" $ "edx"
push = makeMonadicOperation GPush
pop = makeMonadicOperation GPop
global = makeMonadicOperation GGlobal
extern = makeMonadicOperation GExtern
call = makeMonadicOperation GCall

makeLabel :: String -> [Code]
makeLabel l = [L . Label $ l]

jmp :: [Code] -> [Code]
jmp (L label:_) = [MO . GJump $ label]
je :: [Code] -> [Code]
je (L label:_) = [MO . GJe $ label]

ret = [ZO GRet]
cdq = [ZO GCdq]

setg = (ZO GSetg) : movzx "eax" "al"
setge = (ZO GSetge) : movzx "eax" "al"
setl = (ZO GSetl) : movzx "eax" "al"
setle = (ZO GSetle) : movzx "eax" "al"
sete = (ZO GSete) : movzx "eax" "al"
setne = (ZO GSetne) : movzx "eax" "al"

common :: String -> Integer -> [Code]
common l n = [BO . GCommon l $ n]

globalVariable :: String -> String
globalVariable name = "[" ++ name ++ "]"

memoryAddress :: String -> Integer -> String
memoryAddress base n | n == 0 = globalVariable base
                     | n > 0 = globalVariable $ base ++ "+" ++ show n
                     | otherwise = globalVariable $ base ++ show n

nLocal :: SStatement -> Integer
nLocal (SCompoundStatement scs) =
    abs . minimum . (++) [0] . map vAddress . declarations $ scs

class CodeGeneration a where
    codeGenerate :: a -> [Code]

instance CodeGeneration SVal where
    codeGenerate (SFunc f) =
        let g = global . name. fname $ f
            lb = makeLabel . name . fname $ f
            fPush = push "ebp"
            fMove = mov "ebp" "esp"
            fSub = if (==) 0 . nLocal . body $ f
                   then []
                   else sub "esp" . show . nLocal . body $ f
            bCode = codeGenerate . body $ f
            rLab = makeLabel . (++) (name . fname $ f) $ "ret"
            rMove = mov "esp" "ebp"
            rPop = pop "ebp"
        in foldr1 (++) [g, lb, fPush, fMove, fSub, bCode, rLab, rMove, rPop, ret]
    codeGenerate (STmpFunc tf) = extern . tmpFname $ tf
    codeGenerate (SDecl d) = common (vname $ d) (sizeOfType . vType $ d)

instance CodeGeneration SCVal where
    codeGenerate (SNumber n) = mov "eax" . show $ n
    codeGenerate (SIdent i) = mov "eax" . memoryAddress "ebp" . address $ i
    codeGenerate (SMinus v) = (++) (codeGenerate v) . imul "eax" $ "-1"
    codeGenerate (SCValList l1 l2) = (++) (codeGenerate l1) . codeGenerate $ l2
    codeGenerate (SCalFunc i v) =
        let vCodes =
                map (reverse . (++) (push "eax") . reverse . codeGenerate) . reverse $ v
        in (++) (foldr (++) [] vCodes) . call . name $ i
    codeGenerate (SAssign i v) =
        let vCode = codeGenerate v
            assign = mov (memoryAddress "ebp" . address $ i) "eax"
        in vCode ++ assign
    codeGenerate (SAdd n1 n2) = generateExpCode add n1 n2
    codeGenerate (SSub n1 n2) = generateExpCode sub n1 n2
    codeGenerate (SMul n1 n2) = generateExpCode imul n1 n2
    codeGenerate (SDiv n1 n2) =
        let code1 = codeGenerate n1
            code2 = codeGenerate n2
        in foldr (++) [] [code1, mov "ecx" "eax", code2, idiv "ecx"]
    codeGenerate (SMod n1 n2) =
        let code1 = codeGenerate n1
            code2 = codeGenerate n2
        in foldr (++) [] [code1, mov "ecx" "eax", code2, imod "ecx"]
    codeGenerate (SMore n1 n2) = generateCmpCode setg n1 n2
    codeGenerate (SLess n1 n2) = generateCmpCode setl n1 n2
    codeGenerate (SMoreE n1 n2) = generateCmpCode setge n1 n2
    codeGenerate (SLessE n1 n2) = generateCmpCode setle n1 n2
    codeGenerate (SEqual n1 n2) = generateCmpCode sete n1 n2
    codeGenerate (SNEqual n1 n2) = generateCmpCode setne n1 n2
    codeGenerate (SL_AND t n1 n2) =
        let fAnd = mov "dword egx" "0"
            code1 = codeGenerate n1
            code2 = codeGenerate n2
            sAnd = mov "dword egx" "1"
            lb = makeLabel t
            c = cmp "eax" "0"
            j = je lb
        in foldr (++) [] [fAnd, code1, c, j, code2, c, j, sAnd, lb, mov "eax" "egx"]
    codeGenerate (SL_OR t n1 n2) =
        let fOr = mov "dword ehx" "1"
            code1 = codeGenerate n1
            code2 = codeGenerate n2
            sOr = mov "dword ehx" "0"
            lb = makeLabel t
            c = cmp "eax" "1"
            j = je lb
        in foldr (++) [] [fOr, code1, c, j, code2, c, j, sOr, lb, mov "eax" "ehx"]
    codeGenerate (TmpVar vo) =
        let eCode = codeGenerate . tmpvExp $ vo
            mCode = mov (memoryAddress "ebp" . vAddress $ vo) $ "eax"
        in foldr (++) [] [eCode, mCode]

generateExpCode :: (String -> String -> [Code]) -> SCVal
                                                -> SCVal
                                                -> [Code]
generateExpCode op n1 n2@(TmpVar var) =
    let code1 = codeGenerate n1
        code2 = codeGenerate n2
        eMov = mov "eax" . memoryAddress "ebp" . vAddress $ var
        opeCode = op "eax" . memoryAddress "ebp" . vAddress $ var
    in foldr (++) [] [code2, eMov, code1, opeCode]
generateExpCode op n1@(TmpVar var) n2 =
    let code1 = codeGenerate n1
        code2 = codeGenerate n2
        eMov = mov "eax" . memoryAddress "ebp" . vAddress $ var
        opeCode = op "eax" . memoryAddress "ebp" . vAddress $ var
    in foldr (++) [] [code1, eMov, code2, opeCode]
generateExpCode op n1 n2 =
    let code1 = codeGenerate n1
        code2 = codeGenerate n2
    in foldr (++) [] [code2, mov "ebx" "eax", code1, op "eax" "ebx"]

generateCmpCode :: [Code] -> SCVal -> SCVal -> [Code]
generateCmpCode op n1 n2 =
    let code = generateExpCode cmp n1 n2
    in code ++ op

instance CodeGeneration SStatement where
    codeGenerate SNullExp = []
    codeGenerate (SExpression var) = codeGenerate var
    codeGenerate (SIf it cond s1 SNullExp) =
        let lb = makeLabel it
            condCode = codeGenerate cond
            c = cmp "eax" "0"
            j = je endlb
            sCode = codeGenerate s1
            endlb = makeLabel . (++) "end" $ it
        in foldr (++) [] [lb, condCode, c, j, sCode, endlb]
    codeGenerate (SIf it cond s1 es) =
        let lb = makeLabel it
            condCode = codeGenerate cond
            c = cmp "eax" "0"
            j = je elselb
            jend = jmp endlb
            sCode = codeGenerate s1
            elselb = makeLabel . (++) "else" $ it
            eCode = codeGenerate es
            endlb = makeLabel . (++) "end" $ it
        in foldr (++) [] [lb, condCode, c, j, sCode, jend, elselb, eCode, endlb]
    codeGenerate (SWhile wt cond s) =
        let lb = makeLabel wt
            condCode = codeGenerate cond
            sCode = codeGenerate s
            endlb = makeLabel . (++) "end" $ wt
            jmpFirst = jmp lb
            c = cmp "eax" "0"
            jmpEnd = je endlb
        in foldr (++) [] [lb, condCode, c, jmpEnd, sCode, jmpFirst, endlb]
    codeGenerate (SReturn rf re) =
        let jRet = jmp . makeLabel . (++) rf $ "ret"
        in case re of
             Nothing -> jRet
             Just expr -> let eCode = codeGenerate expr
                          in foldr (++) [] [eCode, jRet]
    codeGenerate (SDeclaration _) = []
    codeGenerate (SCompoundStatement scs) = foldr (++) [] . map codeGenerate $ scs

generateCode :: GlobalSValTable -> [Code]
generateCode table =
    let decls = Map.elems . Map.map codeGenerate . Map.filter isSDecl $ table
        funcs = Map.elems . Map.map codeGenerate . Map.filter (not . isSDecl) $ table
    in foldr (++) (foldr (++) [] funcs) decls