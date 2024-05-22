import Data.Char
import Data.List
import Data.Maybe

data Token = 
    TokIdent String       -- Ідентифікатор (наприклад, ім'я змінної або функції)
  | TokNum Int            -- Ціле число
  | TokKeyword String     -- Ключове слово (наприклад, "if", "else")
  | TokSingleChar Char    -- Однолітерний роздільник (наприклад, ';', '(', ')')
  | TokDoubleChar String  -- Дволітерний роздільник (наприклад, '==', '!=')
  | TokOperator String    -- Оператор (наприклад, '=', '+', '-')
  | TokEOF                -- Кінець файлу
  deriving (Show, Eq)


-- Службові слова
keywords :: [String]
keywords = ["if", "else", "while", "return", "int", "float", "void"]

-- Однолітерні роздільники
singleCharDelimiters :: [Char]
singleCharDelimiters = ";,(){}[]?"

-- Дволітерні роздільники
doubleCharDelimiters :: [String]
doubleCharDelimiters = ["==", "!=", "<=", ">=", "&&", "||"]

-- Оператори
operators :: [String]
operators = ["+", "-", "*", "/", "=", ">"]

-- Лексичний аналізатор
lexer :: String -> [Token]
lexer [] = [TokEOF]
lexer input@(c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexIdent input
  | isDigit c = lexNum input
  | take 2 input `elem` doubleCharDelimiters = TokDoubleChar (take 2 input) : lexer (drop 2 input)
  | c `elem` singleCharDelimiters = TokSingleChar c : lexer cs
  | [c] `elem` operators = TokOperator [c] : lexer cs
  | otherwise = error $ "Unexpected character: " ++ [c]

-- Лексема для ідентифікаторів та ключових слів
lexIdent :: String -> [Token]
lexIdent cs =
  let (ident, rest) = span isAlphaNum cs
  in if ident `elem` keywords
       then TokKeyword ident : lexer rest
       else case splitIdentifier ident of
              Just (first, second) -> TokIdent first : TokIdent second : lexer rest
              Nothing -> TokIdent ident : lexer rest

-- Функція для розділення ідентифікатора на два токени, якщо він складається з ключового слова та інших символів
splitIdentifier :: String -> Maybe (String, String)
splitIdentifier [] = Nothing
splitIdentifier (x:xs) =
  if isAlpha x && any isDigit xs
    then Just (takeWhile isAlpha xs, dropWhile isAlpha xs)
    else Nothing

-- Лексема для чисел
lexNum :: String -> [Token]
lexNum cs = let (num, rest) = span isDigit cs
            in TokNum (read num) : lexer rest
-- Приклад використання лексичного аналізатора
main :: IO ()
main = do
  let code = "int main() { int a = 10; if (a >= 10) { a = a + 1; } return a; }"
  let tokens = lexer code
  print tokens

  -- Тести
  print $ test1 == [TokKeyword "int", TokIdent "main", TokSingleChar '(', TokSingleChar ')', TokSingleChar '{', TokKeyword "int", TokIdent "a", TokOperator "=", TokNum 10, TokSingleChar ';', TokKeyword "if", TokSingleChar '(', TokIdent "a", TokDoubleChar ">=", TokNum 10, TokSingleChar ')', TokSingleChar '{', TokIdent "a", TokOperator "=", TokIdent "a", TokOperator "+", TokNum 1, TokSingleChar ';', TokSingleChar '}', TokKeyword "return", TokIdent "a", TokSingleChar ';', TokSingleChar '}', TokEOF]
  print $ test2 == [TokKeyword "if", TokSingleChar '(', TokIdent "x", TokDoubleChar "==", TokNum 1, TokSingleChar ')', TokSingleChar '{', TokKeyword "return", TokNum 0, TokSingleChar ';', TokSingleChar '}', TokEOF]
  print $ test3 == [TokKeyword "while", TokSingleChar '(', TokIdent "n", TokOperator ">", TokNum 0, TokSingleChar ')', TokSingleChar '{', TokIdent "n", TokOperator "=", TokIdent "n", TokOperator "-", TokNum 1, TokSingleChar ';', TokSingleChar '}', TokEOF]
  print $ test4 == [TokKeyword "void", TokIdent "foo", TokSingleChar '(', TokKeyword "int", TokIdent "a", TokSingleChar ')', TokSingleChar '{', TokIdent "a", TokOperator "=", TokIdent "a", TokOperator "*", TokNum 2, TokSingleChar ';', TokSingleChar '}', TokEOF]

-- Тести
test1 :: [Token]
test1 = lexer "int main() { int a = 10; if (a >= 10) { a = a + 1; } return a; }"

test2 :: [Token]
test2 = lexer "if (x == 1) { return 0; }"

test3 :: [Token]
test3 = lexer "while (n > 0) { n = n - 1; }"

test4 :: [Token]
test4 = lexer "void foo(int a) {a=a*2;}"
