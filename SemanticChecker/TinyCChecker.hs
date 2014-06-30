module SemanticChecker.TinyCChecker where

import Data.List
import Data.Either
import Data.Maybe    
import qualified Data.Map as Map

import CompileError
import Syntax.AST
import Syntax.Type
import Syntax.Semantic

createTable :: [Program] -> ([CompileLog], GlobalSValTable)
createTable program = collectGlobalSVals Map.empty [] program

collectGlobalSVals :: GlobalSValTable -> [CompileLog]
		      		      -> [Program]
				      -> ([CompileLog], GlobalSValTable)
collectGlobalSVals gtable clog [] = (clog, gtable)
collectGlobalSVals gtable compileLog (PFunc func : xs) =
    let (clog, table) = makeSFunc gtable func
    in  collectGlobalSVals table (compileLog ++ clog) xs
collectGlobalSVals gtable compileLog (PDecl (Declaration varlist):xs) =
    let (clog, gtable') = sub gtable compileLog varlist
    in collectGlobalSVals gtable' (compileLog ++ clog) xs
    where sub table log [] = (log, table)
          sub table log (v : vs) =
              let (log', table') = makeGlobalSDecl table v
                  (log'', table'') = sub table' log' vs
              in (log ++ log'', table'')

makeSFunc :: GlobalSValTable -> Function
                             -> ([CompileLog], GlobalSValTable)
makeSFunc gtable func@(Func tp (Identifier name) ps cs) =
    let tmp = makeSTmpFunction func
        (css, pd) = makeSParameter (addLevel initialState) ps
        (css', cs') = makeSStatement (Map.insert name tmp gtable) css cs
    in case pd of
         Left err ->
             ((++) (compileLog css') . (++) err . foldr (++) [] . lefts $ [cs'], gtable)
         Right val -> case cs' of
                        Left err -> ((++) err . compileLog $ css', gtable)
                        Right val' -> let func = SFunc $ FuncObj (SIdentifier name 0) val (convT tp) val'
                                      in (compileLog css', Map.insert name func gtable)

makeSTmpFunction :: Function -> SVal
makeSTmpFunction (Func tp (Identifier name) (ParamDecl pd) _) =
    STmpFunc TmpFuncObj {
                 tmpFname = name,
                 tmpParams = map varName pd,
                 tmpParamTypes = map (convT . varType) pd,
                 tmpReturnType = convT tp}
    where varName (Variation t (Identifier n)) = n
          varType (Variation t (Identifier n)) = t

makeSParameter :: CollectSValState -> ParamDecl
                                   -> (CollectSValState, Either [CompileLog] [VarObj])
makeSParameter css (ParamDecl decl) = varlistToVarObjlist css decl

makeGlobalSDecl :: GlobalSValTable -> Variation
                                   -> ([CompileLog], GlobalSValTable)
makeGlobalSDecl gtable (Variation tp (Identifier name)) =
    if Map.member name gtable
    then ([Err $ ReDeclaration name], gtable)
    else let decl = SDecl VarObj { vname = name,
                                   vType = convT tp,
                                   vAddress = 0,
                                   vLevel = 0 }
         in ([], Map.insert name decl gtable)

type Stack = [(Integer, Integer, String, Integer)]

addStack :: Stack -> Integer -> Integer -> String -> Stack
addStack s lev address name = (lev, address, name, toInteger . length $ s) : s

redeclCheck :: Stack -> VarObj -> Maybe CompileLog
redeclCheck stack var@(VarObj varname _ _ _) =
    case findSValinStack stack varname of
      (-1, -1, -1) -> Nothing
      (1, _, k) -> if 1 == (vLevel var)
                   then Just $ Err $ ReDeclaration $ varname
                   else Just $ War $ ParamShadow $ varname
      (i, _, k) -> if i == (vLevel var)
                   then Just $ Err $ ReDeclaration $ varname
                   else Nothing

findSValinStack :: Stack -> String -> (Integer, Integer, Integer)
findSValinStack stack str = case find f stack of
    Nothing -> (-1, -1, -1)
    Just (l, a, _, i) -> (l, a, i)
    where f (_, _, x, _) = x == str

modifyStack :: Stack -> Integer -> Stack
modifyStack [] _ = []
modifyStack s@((l,_,_,_):ss) lev | lev < l = modifyStack ss lev
                                 | otherwise = s

data CollectSValState = CSS { stack :: Stack,
                              svalTable :: SValTable,
                              compileLog :: [CompileLog],
                              lev :: Integer }
                        deriving (Show)

insertSVal :: CollectSValState -> SVal -> CollectSValState
insertSVal css sval@(SDecl var) =
    CSS { stack = addStack (stack css) (lev css) (vAddress var) (vname  var),
          svalTable = Map.insert (toInteger $ Map.size $ svalTable css) sval (svalTable css),
          compileLog = compileLog css,
          lev = lev css }

addLevel :: CollectSValState -> CollectSValState
addLevel css =
    CSS { stack = stack css,
          svalTable = svalTable css,
          compileLog = compileLog css,
          lev = lev css + 1 }

downLevel :: CollectSValState -> CollectSValState
downLevel css =
    CSS { stack = modifyStack (stack css) (lev css - 1),
          svalTable = svalTable css,
          compileLog = compileLog css,
          lev = lev css - 1 }

addLog :: CollectSValState -> CompileLog -> CollectSValState
addLog table log = CSS { stack = stack table,
                         svalTable = svalTable table,
                         compileLog = log : compileLog table,
                         lev = lev table }

calcAdr :: SValTable -> Stack -> Integer -> Integer
calcAdr table stack lv
    | lv == 1 = foldl f 8 stack
    | otherwise = foldl g (negate 4) stack
    where f size (_, _, _, k) = (+) size . sizeOf . fromJust . Map.lookup k $ table
          g size (i, _, _, k) | i == 1 = size
                           | otherwise = (-) size . sizeOf . fromJust . Map.lookup k $ table
                             
initialState :: CollectSValState
initialState = CSS { stack = [], svalTable = emptyTable, compileLog = [], lev = 0 }

makeLocalSDecl :: CollectSValState -> Variation -> SVal
makeLocalSDecl css (Variation tp (Identifier name)) =
    SDecl VarObj { vname = name,
                   vType = convT tp,
                   vAddress = calcAdr (svalTable css) (stack css) (lev css),
                   vLevel = lev css }

makeSCVal :: GlobalSValTable -> CollectSValState
                             -> CVal
                             -> (CollectSValState, Either [CompileLog] SCVal)
makeSCVal _ css (Number n) = (css, Right $ SNumber n)
makeSCVal gtable css (Ident (Identifier name)) =
    case findSValinStack (stack css) name of
      (-1, -1, -1) ->
          case Map.lookup name gtable of
            Nothing -> (css, Left [Err . UndefinedVariable $ name])
            Just (SFunc f) -> (css, Left [Err . VariableWithFunctionCall $ name])
            Just (STmpFunc f) -> (css, Left [Err . VariableWithFunctionCall $ name])
            Just (SDecl d) ->
                (css, Right $ SIdent SIdentifier { name = name, address = 0 })
      (l, a, i) -> (css, Right $ SIdent SIdentifier { name = name, address = a })
                
makeSCVal gtable css (Minus val) =
    let (css', val') = makeSCVal gtable css val
      in case val' of
           Left err -> (css', Left err)
           Right v -> (css', Right $ SMinus v)
     
makeSCVal gtable css (CalFunc (Identifier name) param) =
    case Map.lookup name gtable of
      Nothing ->
        let war = addLog css . War . CallUndefineFunction $ name
        in if null logs
           then (war, Right $ SCalFunc (SIdentifier name 0) $ param')
           else (war, Left logs)
      Just (SFunc func) ->
          let expectLength = toInteger . length . params $ func
              givenLength = toInteger . length $ param
          in if expectLength == givenLength
             then if null logs
                  then (css, Right $ SCalFunc (SIdentifier name 0) $ param')
                  else (css, Left logs)
             else (css, Left $ (Err . InvalidNumOfParameter name expectLength $ givenLength) : logs)
      Just (STmpFunc func) ->
          let expectLength = toInteger . length . tmpParams $ func
              givenLength = toInteger . length $ param
          in if expectLength == givenLength
             then if null logs
                  then (css, Right $ SCalFunc (SIdentifier name 0) $ param')
                  else (css, Left logs)
             else (css, Left $ (Err . InvalidNumOfParameter name expectLength $ givenLength) : logs)
      Just (SDecl var) ->
          (css, Left $ (Err . FunctionCallWithVariable $ name) : logs)
    where scvals' = map (makeSCVal gtable css) param
          param' = map fromRight . filter isRight . map snd $ scvals'
          logs = foldr (++) [] . lefts . map snd $ scvals'

makeSCVal gtable css (Assign (Identifier name) val) =
    case findSValinStack (stack css) name of
      (-1, -1, -1) ->
          case Map.lookup name gtable of
            Nothing ->
                let err = Err . UndefinedVariable $ name
                in case scvals' of
                     Left err' ->
                         (css, Left $ err : err')
                     Right _ -> (css, Left [err])
            Just (SDecl d) ->
                case scvals' of
                  Left err' -> (css, Left err')
                  Right val' -> (css, Right $ SAssign (SIdentifier name 0) val')
            Just _ ->
                let err = Err . VariableWithFunctionCall $ name
                in case scvals' of
                     Left err' ->
                         (css, Left $ err : err')
                     Right _ -> (css, Left [err])
      (l, a, i) ->
          case scvals' of
            Left err' -> (css, Left err')
            Right val' -> (css, Right $ SAssign (SIdentifier name a) val')
    where scvals' = snd . makeSCVal gtable css $ val

makeSCVal gtable css (CValList l ls) = makeSCValExpr gtable css l ls SCValList
makeSCVal gtable css (Add val1 val2) = makeSCValExpr gtable css val1 val2 SAdd
makeSCVal gtable css (Sub val1 val2) = makeSCValExpr gtable css val1 val2 SSub
makeSCVal gtable css (Mul val1 val2) = makeSCValExpr gtable css val1 val2 SMul
makeSCVal gtable css (Div val1 val2) = makeSCValExpr gtable css val1 val2 SDiv
makeSCVal gtable css (Mod val1 val2) = makeSCValExpr gtable css val1 val2 SMod
makeSCVal gtable css (More val1 val2) = makeSCValExpr gtable css val1 val2 SMore
makeSCVal gtable css (Less val1 val2) = makeSCValExpr gtable css val1 val2 SLess
makeSCVal gtable css (MoreE val1 val2) = makeSCValExpr gtable css val1 val2 SMoreE
makeSCVal gtable css (LessE val1 val2) = makeSCValExpr gtable css val1 val2 SLessE
makeSCVal gtable css (Equal val1 val2) = makeSCValExpr gtable css val1 val2 SEqual
makeSCVal gtable css (NEqual val1 val2) = makeSCValExpr gtable css val1 val2 SNEqual
makeSCVal gtable css (L_AND val1 val2) = makeSCValExpr gtable css val1 val2 SL_AND
makeSCVal gtable css (L_OR val1 val2) = makeSCValExpr gtable css val1 val2 SL_OR

makeSCValExpr :: GlobalSValTable -> CollectSValState
                                 -> CVal
                                 -> CVal
                                 -> (SCVal -> SCVal -> SCVal)
                                 -> (CollectSValState, Either [CompileLog] SCVal)
makeSCValExpr gtable css cval1 cval2 constructor =
    let (css', val1') = makeSCVal gtable css cval1
        (css'', val2') = makeSCVal gtable css' cval2
        vlist = [val1', val2']
        errs = lefts vlist
        vals = rights vlist
    in if all isRight vlist
       then (css'', Right $ constructor (vals !! 0) (vals !! 1))
       else (css'', Left $ foldr (++) [] errs)

makeSStatement :: GlobalSValTable -> CollectSValState
                                  -> Statement
                                  -> (CollectSValState, Either [CompileLog] SStatement)
makeSStatement _ css (NullExp) = (css, Right SNullExp)

makeSStatement gtable css (Expression val) =
    let (css', scval) = makeSCVal gtable css val
    in case scval of
         Left err -> (css', Left err)
         Right val -> (css', Right $ SExpression val)

makeSStatement gtable css (If cond state1 state2) = f scond
    where (css', scond) = makeSCVal gtable css cond
          (css'', sstate1) = makeSStatement gtable css' state1
          (css''', sstate2) = makeSStatement gtable css'' state2
          statelist = [sstate1, sstate2]
          f (Left err1) =
              (css''', Left $ (++) err1 . foldr (++) [] . lefts $ statelist)
          f (Right condition) =
              if all isRight statelist
              then (css''', Right $ SIf 0 condition ((rights statelist) !! 0) ((rights statelist) !! 1 ))
              else (css''', Left $ foldr (++) [] . lefts $ statelist)

makeSStatement gtable css (While cond state) = f scond
    where (css', scond) = makeSCVal gtable css cond
          (css'', sstate) = makeSStatement gtable css' state
          statelist = [sstate]
          f (Left err1) =
              (css'', Left $ (++) err1 . foldr (++) [] . lefts $ statelist)
          f (Right condition) =
              if all isRight statelist
              then (css'', Right $ SWhile 0 condition $ head . rights $ statelist)
              else (css'', Left $ foldr (++) [] . lefts $ statelist)

makeSStatement gtable css (Return Nothing) = (css, Right $ SReturn 0 Nothing)
makeSStatement gtable css (Return (Just val)) =
    let (css', sval) = makeSCVal gtable css val
    in case sval of
         Left err -> (css', Left err)
         Right val' -> (css', Right $ SReturn 0 $ Just val')

makeSStatement _ css (Declaration decls) =
    let (css', objs) = varlistToVarObjlist css decls
    in case objs of
         Left err -> (css', Left err)
         Right vars -> (css', Right $ SDeclaration vars)

makeSStatement gtable css (CompoundStatement statelist) =
    let (css', state') = stateListToSStateList gtable (addLevel css) statelist
    in case state' of
         Left err -> (downLevel css', Left err)
         Right val -> (downLevel css', Right $ SCompoundStatement val)

varlistToVarObjlist :: CollectSValState -> [Variation]
                                        -> (CollectSValState, Either [CompileLog] [VarObj])
varlistToVarObjlist css [] = (css, Right [])
varlistToVarObjlist css (var : []) =
    let decl@(SDecl obj) = makeLocalSDecl css var
        (css', clog) = checkAndInsert css decl
    in case clog of
         Nothing -> (css', Right [obj])
         Just clog' -> if isErr clog'
                       then (css', Left [clog'])
                       else (css', Right [obj])
varlistToVarObjlist css (var : vars) =
    let (css', var') = varlistToVarObjlist css [var]
        (css'', vars') = varlistToVarObjlist css' vars
        objlist = var' : [vars']
    in if any isLeft objlist
       then (css'', Left $ foldr (++) [] . lefts $ objlist)
       else (css'', Right $ foldr (++) [] . rights $ objlist)

checkAndInsert :: CollectSValState -> SVal
                                   -> (CollectSValState, Maybe CompileLog)
checkAndInsert css decl@(SDecl var) =
    case redeclCheck (stack css) var of
      Nothing -> (insertSVal css decl, Nothing)
      Just clog -> if isErr clog
                   then (css, Just clog)
                   else (addLog (insertSVal css decl) clog, Nothing)

stateListToSStateList :: GlobalSValTable -> CollectSValState
                                         -> [Statement]
                                         -> (CollectSValState, Either [CompileLog] [SStatement])
stateListToSStateList gtable css [] = (css, Right [SNullExp])
stateListToSStateList gtable css (state : []) =
    let (css', state') = makeSStatement gtable css state
    in case state' of
         Left err -> (css', Left err)
         Right val -> (css', Right [val])
stateListToSStateList gtable css (s : ss) =
    let (css', s') = makeSStatement gtable css s
        (css'', ss') = stateListToSStateList gtable css' ss
    in case s' of
         Left err -> (css'', Left $ (++) err . foldr (++) [] . lefts $ [ss'])
         Right val -> case ss' of
                        Left err -> (css'', Left $ err)
                        Right vals -> (css'', Right $ val : vals)
    
isLeft :: Either a b -> Bool
isLeft (Left x) = True
isLeft (Right x) = False

isRight :: Either a b -> Bool
isRight (Left x) = False
isRight (Right x) = True

fromLeft :: Either a b -> a
fromLeft (Left x) = x

fromRight :: Either a b -> b
fromRight (Right x) = x

containLeft :: [Either a b] -> Bool
containLeft = any isLeft

containNothing :: [Maybe a] -> Bool
containNothing = any isNothing
