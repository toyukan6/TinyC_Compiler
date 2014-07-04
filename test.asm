COMMON x 4
GLOBAL f
f
push ebp
mov ebp, esp
sub esp, 44
mov eax, [ebp-8]
mov [ebp-16], eax
mov eax, [ebp-16]
mov eax, [ebp-12]
mov [ebp-20], eax
add eax, [ebp-16]
mov eax, [ebp-24]
mov [ebp-32], eax
mov eax, [ebp-32]
mov eax, [ebp-12]
mov [ebp-36], eax
add eax, [ebp-32]
mov [ebp-40], eax
mov eax, [ebp-40]
mov eax, [ebp-28]
mov [ebp-44], eax
add eax, [ebp-40]
mov eax, [ebp-4]
mov [ebp-12], eax
mov eax, [ebp-12]
mov eax, [ebp+12]
mov [ebp-16], eax
add eax, [ebp-12]
mov [ebp-20], eax
mov eax, [ebp-20]
mov eax, [ebp-8]
mov [ebp-24], eax
add eax, [ebp-20]
mov eax, [ebp-4]
mov [ebp-8], eax
mov eax, [ebp-8]
mov eax, [ebp+12]
mov [ebp-12], eax
add eax, [ebp-8]
fret
mov esp, ebp
pop ebp
ret
GLOBAL fact
fact
push ebp
mov ebp, esp
sub esp, 20
mov eax, 1
mov [ebp-4], eax
factwhile1
mov eax, [ebp+8]
mov [ebp-8], eax
mov eax, [ebp-8]
mov eax, 0
mov [ebp-12], eax
cmp eax, [ebp-8]
setl al
movzx eax, al
cmp eax, 0
je endfactwhile1
mov eax, [ebp-4]
mov [ebp-16], eax
mov eax, [ebp-16]
mov eax, [ebp+8]
mov [ebp-20], eax
imul eax, [ebp-16]
mov [ebp-4], eax
mov eax, [ebp+8]
mov [ebp-16], eax
mov eax, [ebp-16]
mov eax, 1
mov [ebp-20], eax
sub eax, [ebp-16]
mov [ebp+8], eax
jmp factwhile1
endfactwhile1
mov eax, [ebp-4]
jmp factret
factret
mov esp, ebp
pop ebp
ret
GLOBAL g
g
push ebp
mov ebp, esp
sub esp, 4
mov eax, [ebp+8]
push eax
mov eax, [ebp]
push eax
call f
mov eax, [ebp-4]
push eax
call g
gret
mov esp, ebp
pop ebp
ret
GLOBAL gcd
gcd
push ebp
mov ebp, esp
sub esp, 24
gcdif6
mov eax, [ebp+8]
mov [ebp-4], eax
mov eax, [ebp-4]
mov eax, [ebp+12]
mov [ebp-8], eax
cmp eax, [ebp-4]
sete al
movzx eax, al
cmp eax, 0
je endgcdif6
mov eax, [ebp+8]
jmp gcdret
jmp endgcdif6
elsegcdif6
gcdif5
mov eax, [ebp+8]
mov [ebp-12], eax
mov eax, [ebp-12]
mov eax, [ebp+12]
mov [ebp-16], eax
cmp eax, [ebp-12]
setl al
movzx eax, al
cmp eax, 0
je endgcdif5
mov eax, [ebp+12]
push eax
mov eax, [ebp+8]
mov [ebp-20], eax
mov eax, [ebp-20]
mov eax, [ebp+12]
mov [ebp-24], eax
sub eax, [ebp-20]
push eax
call gcd
jmp gcdret
jmp endgcdif5
elsegcdif5
mov eax, [ebp+12]
mov [ebp-20], eax
mov eax, [ebp-20]
mov eax, [ebp+8]
mov [ebp-24], eax
sub eax, [ebp-20]
push eax
mov eax, [ebp+8]
push eax
call gcd
jmp gcdret
endgcdif5
endgcdif6
gcdret
mov esp, ebp
pop ebp
ret
