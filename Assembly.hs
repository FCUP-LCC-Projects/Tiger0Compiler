module Assembly where

import Lexer
import Parser
import Interm

type Assembly = String

atomToAssem :: Instr -> [Assembly]
atomToAssem (MOVE t1 t2) = ["move $" ++ t1 ++ ",$" ++ t2]
atomToAssem (MOVEI t1 v) = ["li $" ++ t1 ++ "," ++ (show v)]
atomToAssem (OP Add t1 t2 t3) = ["add $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (OP Sub t1 t2 t3) = ["sub $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (OP Mult t1 t2 t3) = ["mul $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (OP Div t1 t2 t3) = ["div $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (OP Mod t1 t2 t3) = ["rem $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (CONDOP Equal t1 t2 t3) = ["seq $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (CONDOP NotEq t1 t2 t3) = ["sne $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (CONDOP GreatEq t1 t2 t3) = ["sge $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (CONDOP LessEq t1 t2 t3) = ["sle $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (CONDOP Lesser t1 t2 t3) = ["slt $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (CONDOP Greater t1 t2 t3) = ["sgt $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
atomToAssem (ANDOP t1 t2 t3) = ["mul $" ++ t1 ++ ",$" ++ t2 ++ ",$" ++ t3]
                          ++ ["sne $" ++ t1 ++ ",$" ++ t1 ++ ",0"]
atomToAssem (OROP t1 t2 t3) = ["sne $" ++ t1 ++ ",$" ++ t3 ++ ",0"]
                      ++ ["or $" ++ t1 ++ ",$" ++ t1 ++ ",$" ++ t2]
atomToAssem (LABEL ('F':xs)) = ["_" ++ xs ++ ":"] ++ ["addi $sp, $sp, -4"] ++ ["sw $ra, 0($sp)"]
atomToAssem (LABEL l) = ["_" ++ l ++ ":"]
atomToAssem (JUMP ('F':xs)) = ["j _" ++ xs]
atomToAssem (JUMP l) = ["j _" ++ l]
atomToAssem (COND t1 Equal t2 l1 l2) = ["bne $" ++ t1 ++ ",$" ++ t2 ++ ",_" ++ l2]
atomToAssem (COND t1 NotEq t2 l1 l2) = ["beq $" ++ t1 ++ ",$" ++ t2 ++ ",_" ++ l2]
atomToAssem (COND t1 GreatEq t2 l1 l2) = ["blt $" ++ t1 ++ ",$" ++ t2 ++ ",_" ++ l2]
atomToAssem (COND t1 LessEq t2 l1 l2) = ["bgt $" ++ t1 ++ ",$" ++ t2 ++ ",_" ++ l2]
atomToAssem (COND t1 Lesser t2 l1 l2) = ["bge $" ++ t1 ++ ",$" ++ t2 ++ ",_" ++ l2]
atomToAssem (COND t1 Greater t2 l1 l2) = ["ble $" ++ t1 ++ ",$" ++ t2 ++ ",_" ++ l2]
atomToAssem (UCOND t1 l1 l2) = ["beq $" ++ t1 ++ ",0,_" ++ l2]
atomToAssem (CALL dest "printi" args) = ["li $v0, 1"] ++ ["syscall"]
atomToAssem (CALL dest "scani" args) = ["li $v0, 5"] ++ ["syscall"] ++ ["move $" ++ dest ++ ",$v0"]
atomToAssem (CALL dest id args) = ["jal _" ++ id]
atomToAssem (RETURN t1) = ["lw $ra, 0($sp)"] ++ ["addi $sp, $sp, 4"] ++ ["jr $ra"]
atomToAssem (STOREREG t1) = ["addi $sp, $sp, -4"] ++ ["sw $" ++ t1 ++ ", 0($sp)"]
atomToAssem (LOADREG t1) = ["lw $" ++ t1 ++ ", 0($sp)"] ++ ["addi $sp, $sp, 4"]

isReturn :: Instr -> Bool
isReturn (RETURN _) = True
isReturn _ = False

takeFun :: [Instr] -> [Instr]
takeFun [] = []
takeFun ((LABEL ('F':xs')):xs) = (takeWhile (\i -> not (isReturn i)) ((LABEL ('F':xs')):xs)) ++ takeFun xs
takeFun ((RETURN r):xs) = (RETURN r):(takeFun xs)
takeFun (x:xs) = takeFun xs

dropFun :: [Instr] -> [Instr]
dropFun [] = []
dropFun ((LABEL ('F':xs')):xs) = dropFun (dropWhile (\i -> not (isReturn i)) ((LABEL ('F':xs')):xs))
dropFun ((RETURN r):xs) = dropFun xs
dropFun (x:xs) = x:(dropFun xs)

getAssembly :: [Instr] -> [Assembly]
getAssembly ls = concat (map atomToAssem ([LABEL "main"] ++ dropFun ls))
                 ++ ["li $v0, 10"] ++ ["syscall"] ++ concat (map atomToAssem (takeFun ls))




