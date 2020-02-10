
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateTypeScript (module GenerateTypeScript) where

import Utility
import Syntax
import LALRAutomaton
import LRTable
import CodeGenerateEnv

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List             (intercalate)

import Data.Monoid           (Endo())
import Control.Monad         (forM_)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe            (mapMaybe)
import Data.Either           (isRight)
import Data.List             (groupBy, mapAccumL)
import Data.Function         (fix)

import Debug.Trace

-------------------------------------------------------------------------------

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellsLn (replicate 79 '/')

-------------------------------------------------------------------------------

tellTypeScript :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellTypeScript syntax = let automaton = lalrAutomaton syntax in
  (`runReaderT` buildCodeGenerateEnv syntax automaton) $ do
    let moduleName = pascalCase (syntaxName syntax)
    tellNewline
    tellSeparator
    tellNewline
    sequenceWithSep_ (tellNewline >> tellSeparator >> tellNewline) $
      [tellGrammar,
       tellUtils,
       tellASTDefinitions,
       tellAutomatonStates,
       tellTransitions,
       tellInitialState]
    tellNewline
    tellSeparator
    tellNewline

-------------------------------------------------------------------------------

tellUtils :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
             m ()
tellUtils = do
  tellsLn "// util scripts"
  tellNewline
  tellsLn utilScripts

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
               m ()
tellGrammar = do
  syntax <- syntax_
  tellsLn "// grammar definition"
  tellNewline
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "// " >> tellRule rule >> tellNewline

-------------------------------------------------------------------------------

tellASTDefinitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                     m ()
tellASTDefinitions = do
  syntax <- syntax_
  tellsLn "// AST nodes"
  tellNewline
  -- AST abstract classes
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    tellsLn $ "interface " ++ pascalCase (nonTerminalName nt) ++ " {"
    tellsLn "\taccept(v? : Visitor): void"
    tellsLn "}"

  tellNewline

  -- AST concrete classes
  forMWithSep_ (tellNewline) (syntaxNonTerminals syntax) $ \nt -> do
    forMWithSep_ (tellNewline) (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      let args = [(typ, "arg" ++ show i) |
                  (i, typ) <- zip [1 ..] (concat (ruleParams rule))]
      tellsLn $ "export class " ++ className ++ " implements " ++ pascalCase (nonTerminalName nt) ++ " {"
      forM_ args $ \(argType, argName) -> do
        tellsLn $ "\t" ++ argName ++ " : " ++ argType
      case args of
        [] -> return ()
        _  -> do
          tells "\tconstructor("
          forMWithSep_ (tells ", ") args $ \(argType, argName) -> do
            tells $ argName ++ " : " ++ argType
          tellsLn ") {"
          forM_ args $ \(argType, argName) -> do
            tellsLn $ "\t\tthis." ++ argName ++ " = " ++ argName
          tellsLn "\t}"
      tellsLn "\taccept(v? : Visitor) {"
      tellsLn "\t\tif (v) {"
      tellsLn $ "\t\t\tv.visit" ++ className ++ "(this)"
      tellsLn "\t\t} else {"
      tellsLn $ "\t\t\tnew DefaultVisitor().visit" ++ className ++ "(this)"
      tellsLn "\t\t}"
      tellsLn "\t}"
      tellsLn "}"

  tellNewline

  -- AST visitors
  tellsLn "interface Visitor {"
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      tellsLn $ "\tvisit" ++ className ++ "(host : " ++ className ++ "): void"
  tellsLn "}"

  tellNewline

  tellsLn "export class DefaultVisitor implements Visitor {"
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      tellsLn $ "\tvisit" ++ className ++ "(host : " ++ className ++ ") {"
      _ <- (`execStateT` 0) $ forMWithSep_ (tellsLn $ "\t\tprocess.stdout.write(\" \")") (zip [1 ..] (ruleRhs rule)) $ \(i, sym) -> case sym of
        NonTerminalSymbol nt -> do
          j <- modify (\c -> c + 1) >> get
          tellsLn $ "\t\thost.arg" ++ show j ++ ".accept(this)"
        TerminalSymbol t -> do
          if 0 < (length $ terminalParams t)
          then
            forMWithSep_ (tellsLn $ "\t\tprocess.stdout.write(\" \")") (zip [1 ..] (terminalParams t)) $ \(k, param) -> do
              modify (\c -> c + 1)
              tellsLn $ "\t\tprocess.stdout.write(\"\"+host.arg" ++ show k ++ ")"
          else
            tellsLn $ "\t\tprocess.stdout.write(\"" ++ (terminalName t) ++ "\")" 
      tellsLn "\t}"
  tellsLn "}"

-------------------------------------------------------------------------------

tellAutomatonStates :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => m ()
tellAutomatonStates = do
  nodes <- nodes_
  tellsLn "// automaton states"
  tellNewline
  tells "type Node = "
  tellsLn $ intercalate " | " $ map (\(node, name, typ) -> name) nodes
  tellNewline
  forMWithSep_ tellNewline nodes $ \(node, name, typ) -> do
    tells $ "class " ++ name ++ " {"
    let brand = "public _" ++ name ++ "Brand: boolean = true"
    params <- nodeParams_ node
    case params of
      [] -> tells $ " " ++ brand ++ " "
      _  -> do
        tellsLn $ "\n\t" ++ brand
        tells ("\tconstructor(")
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells ("public arg" ++ show i ++ " : " ++ param)
        tellsLn ") {}"
    tellsLn "}"

-------------------------------------------------------------------------------

tellTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => m ()
tellTransitions = do
  table <- lrTable_
  tellsLn "// transitions"
  tellNewline
  -- type guards
  s <- (`execStateT` (Set.empty :: Set.Set [String])) $ forM_ (lrTableTransitions table) $ \(src, t, action) -> case action of
    Shift  dst  -> do
      srcName <- pascalCase <$> nodeName_ src
      modify $ Set.insert [srcName]
    Reduce rule -> do
      reduces <- reducesFrom_ src rule
      forM_ reduces $ \(srcPath, dstPath) -> do
        path <- mapM nodeName_ srcPath
        modify $ Set.insert $ path
    Accept      -> do
      srcName <- nodeName_ src
      modify $ Set.insert [srcName]
  mapM tellTypeGuards $ Set.toList s
  -- fluent type
  tells "type Fluent<Stack extends unknown[]> = ("
  forMWithSep_ (tells ")\n\t& (") (lrTableTransitions table) $ \(src, t, action) -> case action of
    Shift  dst  -> tellShiftFluentType  src t dst
    Reduce rule -> tellReduceFluentType src t rule
    Accept      -> tellAcceptFluentType src
  tellsLn ")"
  -- fluent implementation
  tellNewline
  tellsLn "class FluentImpl {"
  tellsLn "\tstack: Node[] = [new Node1]"
  --let allFluentImpl = mapM getFluentImpl $ lrTableTransitions table

  fluentImplList <- mapM getReduceAndAcceptFluentImpl $ lrTableTransitions table
  let fluentImplMap = Map.fromListWith (++) $ concat $ fluentImplList
  shiftFluentImplList <- mapM getShiftFluentImpl $ lrTableTransitions table
  let shiftFluentImplMap =  Map.fromList $ concat $ shiftFluentImplList
  let funNameBodyPair = Map.toAscList $ Map.unionWith (++) fluentImplMap shiftFluentImplMap
  forM_ funNameBodyPair $ \(funName, impl) -> do
    tellsLn $ "\t" ++ funName ++ " = (...a: any[]) => {"
    tells impl
    tellsLn "\t}"
  tellsLn "}"

getReduceAndAcceptFluentImpl :: (MonadReader CodeGenerateEnv m)
  => (LRNode, Terminal, LRAction) -> m [(String, String)]
getReduceAndAcceptFluentImpl (src, t, action) = case action of
  Shift  dst  -> return []
  Reduce rule -> do
    reduces <- reducesFrom_ src rule
    reduceh <- mapM (getReduceFluentImplList t rule) $ reduces
    return reduceh
  Accept      -> do
    accepth <- getAcceptFluentImplList src
    return [accepth]

getShiftFluentImpl :: (MonadReader CodeGenerateEnv m)
  => (LRNode, Terminal, LRAction) -> m [(String, String)]
getShiftFluentImpl (src, t, action) = case action of
  Shift  dst  -> do
    sfl <- getShiftFluentImplList src t dst
    return [sfl]
  _           -> return []

-- Add shift transition later to merge each of them
-- addShiftTransition :: (MonadReader CodeGenerateEnv m)
--   => Map String String -> (LRNode, Terminal, LRAction) -> m Map String String
-- addShiftTransition (src, t, action) M = case action of
--   Shift dst -> Map.insertWith
--   _         -> return M

getShiftFluentImplList :: (MonadReader CodeGenerateEnv m)
  => LRNode -> Terminal -> LRNode -> m (String, String)
getShiftFluentImplList src t dst = do
  srcName <- pascalCase <$> nodeName_ src
  dstName <- pascalCase <$> nodeName_ dst
  let params = terminalParams t
  let args = intercalate ", " ["a[" ++ show i ++ "] as " ++ typ | (i, typ) <- zip [0 ..] params]
  return (terminalName t, "\t\tthis.stack = [new " ++ dstName ++
    "(" ++ args ++ "), ...this.stack]\n" ++
    "\t\treturn this\n")

getReduceFluentImplList :: (MonadReader CodeGenerateEnv m)
  => Terminal -> Rule -> ([LRNode], [LRNode]) -> m (String, String)
getReduceFluentImplList t rule (srcPath, dstPath) = do
  let className = pascalCase (ruleName rule)
  dstName <- pascalCase <$> nodeName_ (head dstPath)
  typeguard <- do path <- mapM nodeName_ srcPath
                  return $ "startsWith" ++ concat path
  let funName = terminalName t
  let params = terminalParams t

  (n, xs) <- (`execStateT` (0, "")) $ forM_ (zip [1 ..] (ruleRhs rule)) $ \(i, sym) -> case sym of
    NonTerminalSymbol nt -> do
      (j, s) <- modify (\(c, ss) -> (c + 1, ss)) >> get
      put $ (j, s ++ "\t\t\tconst x" ++ show j ++ " = this.stack[" ++ show (length (ruleRhs rule) - i) ++ "].arg1\n")
    TerminalSymbol t -> do
      forM_ (zip [1 ..] (terminalParams t)) $ \(k, param) -> do
        (j, s) <- modify (\(c, ss) -> (c + 1, ss)) >> get
        put $ (j, s ++ "\t\t\tconst x" ++ show j ++ " = this.stack[" ++ show (length (ruleRhs rule) - i) ++ "].arg" ++ show k ++ "\n")
  let content = "\t\t\tconst content = new " ++ (pascalCase $ ruleName rule) ++ "(" ++ intercalate ", " ["x" ++ show i | i <- [1 .. n]] ++ ")\n"
  let tail = "\t\t\tconst tail = this.stack.slice(" ++ show (length srcPath - 1) ++ ")\n"
  return (funName, "\t\tif (" ++
    typeguard ++ "(this.stack)) {\n" ++ xs ++
    content ++ tail ++
    "\t\t\tthis.stack = [new " ++ dstName ++
    "(content), ...tail]\n" ++
    "\t\t\treturn this." ++ funName ++ "()\n" ++ "\t\t}\n")

getAcceptFluentImplList :: (MonadReader CodeGenerateEnv m)
  => LRNode -> m (String, String)
getAcceptFluentImplList src = do
  srcName <- nodeName_ src
  return ("end", "\t\tif (startsWith" ++
    srcName ++ "(this.stack)) {\n" ++
    "\t\t\treturn this.stack[0].arg1\n" ++ "\t\t}")

tellReduceFluentType :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> Terminal -> Rule -> m ()
tellReduceFluentType src t rule = do
  reduces <- reducesFrom_ src rule
  forMWithSep_ (tells ") & (") reduces $ \(srcPath, dstPath) -> do
    dstName <- pascalCase <$> nodeName_ (head dstPath)
    condition <- do path <- mapM nodeName_ srcPath
                    return $ "[StartsWith<Stack, [" ++ intercalate ", " path ++ "]>]"
    dstType <- do path <- mapM nodeName_ dstPath
                  return $ "Fluent<Prepend<" ++ dstName ++ ", " ++
                    concat ["Tail<" | _ <- tail srcPath] ++ "Stack" ++
                    concat [">" | _ <- tail srcPath] ++ ">>"
    let funName = terminalName t
    let params = terminalParams t
    let args = intercalate ", " ["arg" ++ show i ++ ": " ++ typ | (i, typ) <- zip [1 ..] params]
    tells "{"
    tells " 0: {},"
    tells $ " 1: " ++ dstType ++ "extends { " ++ funName ++ ": infer F }"
    tells $ " ? { " ++ funName ++ ": F }"
    tells " : {}"
    tells "}"
    tells condition

tellShiftFluentType :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                    => LRNode -> Terminal -> LRNode -> m ()
tellShiftFluentType src t dst = do
  srcName <- pascalCase <$> nodeName_ src
  dstName <- pascalCase <$> nodeName_ dst
  let funName = terminalName t
  let dstType = "Fluent<Prepend<" ++ dstName ++ ", Stack>>"
  let params = terminalParams t
  let args = intercalate ", " ["arg" ++ show i ++ ": " ++ typ | (i, typ) <- zip [1 ..] params]
  tells "{"
  tells " 0: {},"
  tells $ " 1: { " ++ funName ++ ": (" ++ args ++ ") => " ++ dstType ++ " }"
  tells "}"
  tells $ "[StartsWith<Stack, [" ++ srcName ++ "]>]"

tellAcceptFluentType :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> m ()
tellAcceptFluentType src = do
  srcName <- nodeName_ src
  tells "{"
  tells " 0: {},"
  tells $ " 1: { end: () => " ++ srcName ++ "['arg1']" ++ " }"
  tells "}"
  tells $ "[StartsWith<Stack, [" ++ srcName ++ "]>]"

tellTypeGuards ::  (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => [String] -> m ()
tellTypeGuards nodes = do
  tellsLn $ "function startsWith" ++ (concat nodes) ++ "(arg: any): arg is AddUnknownNodeRest<[" ++ (intercalate ", " nodes) ++ "]> {"
  tells "\treturn "
  forMWithSep_ (tells " && ") (zip [0 ..] nodes) $ \(i, node) -> do
    tells $ "arg[" ++ show i ++ "] && arg[" ++ show i ++ "]._" ++ node ++ "Brand"
  tellsLn "\n}"
  tellNewline

-------------------------------------------------------------------------------

tellInitialState :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                 => m ()
tellInitialState = do
  automaton <- automaton_
  startName <- pascalCase <$> nodeName_ (lrAutomatonStart automaton)
  tellsLn $ "export function begin(): Fluent<[" ++ startName ++ "]> {"
  tellsLn $ "\treturn new FluentImpl() as any"
  tellsLn "}"

-------------------------------------------------------------------------------

utilScripts = "type Length<T extends unknown[]> = T['length']\n\
\type Prepend<Elm, T extends unknown[]> = ((\n\
\\targ: Elm,\n\
\\t...rest: T\n\
\) => void) extends ((...args: infer T2) => void)\n\
\\t? T2\n\
\\t: never\n\
\\n\
\type Rest<T extends unknown[]> = ((\n\
\\t...rest: T\n\
\) => void) extends ((head: unknown, ...args: infer T2) => void)\n\
\\t? T2\n\
\\t: never\n\
\type Tail<T extends any[]> = ((...args: T) => any) extends ((\n\
\\t_: infer First,\n\
\\t...rest: infer R\n\
\) => any)\n\
\\t? T extends any[] ? R : ReadonlyArray<R[number]>\n\
\\t: []\n\
\declare const None: unique symbol\n\
\type None = typeof None\n\
\type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]\n\
\type AddUnknownNodeRest<Tuple extends Node[], Result extends Node[] = [...Node[]]> = {\n\
\\tempty: Result,\n\
\\tnonEmpty: ((..._: Tuple) => Node) extends ((_: infer First, ..._1: infer Next) => Node)\n\
\\t\t? Prepend<First, AddUnknownNodeRest<Rest<Tuple>, Result>>\n\
\\t\t: never\n\
\}[\n\
\\tTuple extends [unknown, ...unknown[]]\n\
\\t\t? 'nonEmpty'\n\
\\t\t: 'empty'\n\
\]\n\
\\n\
\type CompareLength<Left extends any[], Right extends any[]> = {\n\
\\tfitBoth: 'equal'\n\
\\tfitLeft: 'shorterLeft'\n\
\\tfitRight: 'shorterRight'\n\
\\tunfit: ((..._: Left) => any) extends ((_: any, ..._1: infer LeftRest) => any) ?\n\
\\t\t ((..._: Right) => any) extends ((_: any, ..._1: infer RightRest) => any) ?\n\
\\t\t\t\t\tCompareLength<LeftRest, RightRest>\n\
\\t\t\t: never\n\
\\t\t\t: never\n\
\}[\n\
\\tLeft['length'] extends Right['length'] ? 'fitBoth' :\n\
\\tLeft extends [] ? 'fitLeft' :\n\
\\tRight extends [] ? 'fitRight' :\n\
\\t'unfit'\n\
\]\n\
\\n\
\type StartsWith<Tuple extends unknown[], Tuple2 extends unknown[]> = {\n\
\\tfalse: 0,\n\
\\tempty: 1,\n\
\\tnonEmpty: Head<Tuple> extends Head<Tuple2>\n\
\\t\t? StartsWith<Rest<Tuple>, Rest<Tuple2>>\n\
\\t\t: 0\n\
\}[\n\
\\tCompareLength<Tuple, Tuple2> extends 'shorterLeft'\n\
\\t\t? 'false'\n\
\\t\t: IsFinite<Tuple2, 'finite', 'infinite'> extends 'infinite'\n\
\\t\t\t? 'false'\n\
\\t\t\t: Tuple2 extends [unknown, ...unknown[]]\n\
\\t\t\t\t? 'nonEmpty'\n\
\\t\t\t\t: 'empty'\n\
\]\n\
\type IsFinite<Tuple extends unknown[], Finite, Infinite> = {\n\
\\tempty: Finite\n\
\\tnonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Rest) => unknown)\n\
\\t\t? IsFinite<Rest, Finite, Infinite>\n\
\\t\t: never\n\
\\tinfinite: Infinite\n\
\}[\n\
\\tTuple extends [] ? 'empty' :\n\
\\tTuple extends (infer Element)[] ?\n\
\\tElement[] extends Tuple ?\n\
\\t\t'infinite'\n\
\\t: 'nonEmpty'\n\
\\t: never\n\
\]"
