module Interm where
import           Lexer
import           Parser
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

type Ident = String
type Temp  = String
type Label = String
type Table = Map Ident String
type Count = (Int,Int)  -- contadores para temporários e etiquetas

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t)

newATemp :: State Count Temp
newATemp = do (t,l)<-get; put (t+1,l); return ("a"++show t)

tempV :: Int -> State Count ()
tempV i = modify (\(t,l) -> (i,l))

popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l) -> (t-k,l))

newLabel :: State Count Label
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)

newFLabel :: Ident -> State Count Label
newFLabel id = do (t,l)<-get; put (t,l+1); return ("F"++id)

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OP BinOp Temp Temp Temp
           | CONDOP RelOp Temp Temp Temp
		   | ANDOP Temp Temp Temp
		   | OROP Temp Temp Temp
           | OPI BinOp Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp RelOp Temp Label Label
		   | UCOND Temp Label Label
		   | STOREREG Ident
		   | LOADREG Ident
           | CALL Temp Label [Temp]
           | RETURN Temp
           deriving Show


transExpr :: Table -> Exp -> Ident -> State Count [Instr]

transExpr tabl (Var x) dest
        = case Map.lookup x tabl of
         Just temp -> return [MOVE dest temp]
         Nothing -> error ("invalid variable "++show x)


transExpr tabl (Num n) dest
       = return [MOVEI dest n]

transExpr tabl (Op op e1 e2) dest
   = do temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExpr tabl e1 temp1
        code2 <- transExpr tabl e2 temp2
        popTemp 2
        return (code1 ++ code2 ++ [OP op dest temp1 temp2])

transExpr tabl (Cond op e1 e2) dest
   = do temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExpr tabl e1 temp1
        code2 <- transExpr tabl e2 temp2
        popTemp 2
        return (code1 ++ code2 ++ [CONDOP op dest temp1 temp2])

transExpr tabl (And e1 e2) dest
  = do temp1 <- newTemp
       temp2 <- newTemp
       code1 <- transExpr tabl e1 temp1
       code2 <- transExpr tabl e2 temp2
       popTemp 2
       return (code1 ++ code2 ++ [ANDOP dest temp1 temp2])

transExpr tabl (Or e1 e2) dest
  = do temp1 <- newTemp
       temp2 <- newTemp
       code1 <- transExpr tabl e1 temp1
       code2 <- transExpr tabl e2 temp2
       popTemp 2
       return (code1 ++ code2 ++ [OROP dest temp1 temp2])

transExpr tabl (Assign var expr) nodest
    = do code <- transAssign tabl var expr
         return code

transExpr tabl (For id e1 e2 e3) nodest
    = do ltrue <- newLabel
         lfalse <- newLabel
         let tabl2 = insertVar tabl (DeclVar (Declare id e1)) nodest
         code0 <- transAssign tabl2 id e1
         code1 <- transExpr tabl2 e2 nodest
         code2 <- transExpr tabl2 e3 nodest
         return (code0 ++ [LABEL ltrue] ++ code1 ++
            code2 ++ [JUMP ltrue, LABEL lfalse])

transExpr tabl (IfThen cond stm1) dest
    = do ltrue  <- newLabel
         lfalse <- newLabel
         code0  <- transCond tabl cond ltrue lfalse
         code1  <- transExpr tabl stm1 dest
         return (code0 ++ [LABEL ltrue] ++
             code1 ++ [LABEL lfalse])

transExpr tabl (IfThenElse cond stm1 stm2) dest
       = do ltrue <- newLabel
            lfalse <- newLabel
            lend <- newLabel
            code0 <- transCond tabl cond ltrue lfalse
            code1 <- transExpr tabl stm1 dest
            code2 <- transExpr tabl stm2 dest
            return (code0 ++ [LABEL ltrue] ++ code1 ++
                    [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transExpr tabl (WhileDo cond stm) nodest =
       do lbody <- newLabel
          lend <- newLabel
          lcond <- newLabel
          code1 <- transExpr tabl stm nodest
          code2 <- transCond tabl cond lbody lend
          return ([LABEL lcond] ++ code2 ++
                 [LABEL lbody] ++ code1 ++ [JUMP lcond] ++ [LABEL lend])

transExpr tabl (Block exps) dest
     = do (code, temps) <- transArgs tabl exps
          popTemp (length exps)
          return code

transExpr tabl (LetInEnd [] []) dest = return []

transExpr tabl (LetInEnd [] (exp:exps)) dest
  = do code1 <- transExpr tabl exp dest
       code2 <- transExpr tabl (LetInEnd [] exps) dest
       return (code1 ++ code2)

transExpr tabl (LetInEnd ((Declare id exp):vdecls) exps) dest
  = do temp <- newTemp
       let tabl2 = insertVar tabl (DeclVar (Declare id exp)) temp
       code1 <- transExpr tabl exp (getVar id tabl2)
       code2 <- transExpr tabl2 (LetInEnd vdecls exps) dest
       return (code1 ++ code2)

transExpr tabl (CallFunct id args) dest
      = do (t,l) <- get
           tempV 0
           (code1, temps) <- transArgs2 tabl args t
           tempV t
           code2 <- storeReg t dest
           popTemp (length args)
           code3 <- getReg t dest
           return (code1 ++ code2 ++ [CALL dest id temps] ++ code3)

storeReg :: Int -> Ident -> State Count [Instr]
storeReg n dest
       = do (t,l) <- get
            return (case n of
               0 -> []
               _ -> (case (dest /= [] && tail dest == show n) of
                  True -> evalState (storeReg (n-1) dest) (t,l)
                  False -> [STOREREG ("t"++show n)] ++ evalState (storeReg (n-1) dest) (t,l) ))

getReg :: Int -> Ident -> State Count [Instr]
getReg n dest
       = do (t,l) <- get
            return (case n of
               0 -> []
               _ -> (case (dest /= [] && tail dest == show n) of
                  True -> evalState (getReg (n-1) dest) (t,l)
                  False -> [LOADREG ("t"++show n)] ++ evalState (getReg (n-1) dest) (t,l) ))

transAssign tabl var expr
   = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> do temp <- newTemp
                      code <- transExpr tabl expr temp
                      return (code ++ [MOVE dest temp])

transArgs tabl args  = worker args
     where
         worker []  = return ([], [])
         worker (exp:exps)
          = do temp <- newTemp
               code <- transExpr tabl exp temp
               (code', temps') <- worker exps
               return (code++code', temp:temps')

transArgs2 tabl args t   = worker args
     where
         worker []  = return ([], [])
         worker (exp:exps)
           = do temp <- newATemp
                tempV t
                code <- transExpr tabl exp temp
                tempV 0
                (code', temps') <- worker exps
                return (code++code', temp:temps')

transCond :: Table -> Exp -> Label -> Label -> State Count [Instr]
transCond tabl (Cond rel e1 e2) ltrue lfalse
   =  do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExpr tabl e1 temp1
         code2 <- transExpr tabl e2 temp2
         popTemp 2
         return ( code1 ++ code2 ++
              [COND temp1 rel temp2 ltrue lfalse] )

transCond tabl (And cond1 cond2) ltrue lfalse
  = do label <- newLabel
       code1 <- transCond tabl cond1 label lfalse
       code2 <- transCond tabl cond2 ltrue lfalse
       return (code1 ++ [LABEL label] ++ code2)

transCond tabl (Or cond1 cond2) ltrue lfalse
  = do label <- newLabel
       code1 <- transCond tabl cond1 ltrue label
       code2 <- transCond tabl cond2 ltrue lfalse
       return (code1 ++ [LABEL label] ++ code2)

transCond tabl e1 ltrue lfalse
  = do temp <- newTemp
       code1 <- transExpr tabl e1 temp
       popTemp 1
       return ( code1 ++ [UCOND temp ltrue lfalse])
 

transDecl :: Table -> Decl -> State Count [Instr]
transDecl tabl (DeclVar var) =
      do code1 <- transVarDecl tabl var
         return code1

transDecl tabl (DeclFunct func) =
   do code1 <- transFunct tabl func
      return code1

transProg :: Table -> Prog -> State Count [Instr]
transProg tabl (LetIn [] []) = return []

transProg tabl (LetIn [] (exp:exps))
  = do code1 <- transExpr tabl exp []
       code2 <- transProg tabl (LetIn [] exps)
       return (code1 ++ code2)

transProg tabl (LetIn [decl] exps)
  = do temp <- newTemp
       label <- newLabel
       let tabl2 = insertVar tabl decl temp
       code1 <- transDecl tabl2 decl
       code2 <- transProg tabl2 (LetIn [] exps)
       return (code1 ++ [LABEL label] ++ code2)

transProg tabl (LetIn (decl:decls) exps)
  = do temp <- newTemp
       let tabl2 = insertVar tabl decl temp
       code1 <- transDecl tabl2 decl
       code2 <- transProg tabl2 (LetIn decls exps)
       return (code1 ++ code2)


insertVar :: Table -> Decl -> String -> Table
insertVar tabl (DeclVar (Declare id exp)) temp = Map.insert id temp tabl

insertVar tabl (DeclFunct (Procedure id args exp)) temp
    = Map.insert id temp tabl


insertVar tabl (DeclFunct (Function id args field exp)) temp
    = Map.insert id temp tabl


insertTable :: Table -> [(Ident,String)] -> Table
insertTable tabl [] = tabl
insertTable tabl ((x,temp):xs) =  insertTable ( Map.insert x temp tabl) xs

transVarDecl :: Table -> VarDecl -> State Count [Instr]
transVarDecl tabl (Declare id exp)
  = do code1 <- transExpr tabl exp (getVar id tabl)
       return code1

getVar :: Ident -> Table -> Ident

getVar v tabl = case Map.lookup v tabl of
         Just temp -> temp
         Nothing -> error ("invalid variable "++show v)

transFunct :: Table -> Funct -> State Count [Instr]
transFunct tabl (Function id args typeid exp)
  = do label <-newFLabel id
       (t,l) <- get
       tempV 0
       temps <- sequence [newATemp | (Field x _) <- args]
       let tabl2 = insertTable tabl (zip [x | (Field x _) <- args] temps)
       tempV t
       temp <- newTemp
       code <-transExpr tabl2 exp temp
       return ([LABEL label]++code++[RETURN temp])

transFunct tabl (Procedure id args exp)
  = do label <-newFLabel id
       (t,l) <- get
       tempV 0
       temps <- sequence [newATemp | (Field x _) <- args]
       let tabl2 = insertTable tabl (zip [x | (Field x _) <- args] temps)
       tempV t
       temp <- newTemp
       code <-transExpr tabl2 exp temp
       return ([LABEL label]++code)
