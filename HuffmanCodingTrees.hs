-- Andrew Fuller
-- JMU Spring '16
-- Huffman Coding Tree
-- Credit for the setup and test cases goes to Dr. Chris Fox of James Madison University

-- Set up --------------------------------------------
import Data.List (sortBy, insertBy)  -- for sorting lists of character counts
import qualified Data.Map as Map     -- for recording Huffman coding tables

-- Define a Huffman tree type
data Tree = Nil | Node (Maybe Char) Int Tree Tree deriving (Show, Eq)

-- Sort a list of character count occurrences
sortCharCounts :: [(Char, Int)] -> [(Char, Int)]
sortCharCounts = sortBy (\(_,cnt1) (_,cnt2) -> compare cnt1 cnt2)

-- Real work starts here -----------------------------

-- Count the occurrences of the characters in a string
countChars :: String -> [(Char, Int)]
countChars n = Map.toList(Map.fromListWith (+) [(char,1) | char <- n ]) 

-- Make a list of Huffman tree leaves from a list of character counts
makeTreeList :: [(Char, Int)] -> [Tree]
makeTreeList n = map countChars n
 where countChars (x,y) = Node (Just x) y Nil Nil

--or, recursive way
--makeTreeList [] = []
--makeTreeList ((char,count):xs) = (Node (Just char) count Nil Nil): makeTreeList xs

-- Make an ordered list of Huffman tree leaves from a list of character counts
makeOrderedTreeList :: [(Char, Int)] -> [Tree]
makeOrderedTreeList = makeTreeList . sortCharCounts

-- Insert a Huffman tree into an ordered list of Huffman trees
-- pre: the tree list is ordered and does not contain Nil
treeListInsert :: Tree -> [Tree] -> [Tree]
treeListInsert x y = insertBy f x y
 where f(Node _ x _ _) (Node _ y _ _) = compare x y

-- Build a Huffman tree from a list of Huffman trees
-- pre: the tree list is ordered and does not contain Nil
buildTreeFromList :: [Tree] -> Tree
buildTreeFromList [] = Nil
buildTreeFromList [x] = (x)
buildTreeFromList (x:y:xs) = buildTreeFromList (treeListInsert f xs)
 where 
  (Node _ n _ _) = x
  (Node _ m _ _) = y
  f = Node Nothing (n+m) x y

-- Create a Huffman tree from a string
-- pre: the string has at least two distinct characters
makeHuffmanTree :: String -> Tree
makeHuffmanTree n = buildTreeFromList(makeOrderedTreeList(countChars n))


-- Produce the Huffman encoding table (as a Map) for a string
makeHuffmanTable :: String -> Map.Map Char String
makeHuffmanTable n = Map.fromList . walk "" $ (makeHuffmanTree n)
 where
  walk _ Nil = []
  walk leaves (Node (Just char) _ _ _) = [(char, leaves)]
  walk leaves (Node Nothing _ l r) = (walk(leaves ++ "0") l) ++ (walk(leaves ++ "1") r) 


-- Encode a string into a bit string using Huffman encoding
-- pre: the string has at least two distinct characters
encode :: String -> String
encode string = concat $ map (table Map.!) string
 where table = makeHuffmanTable string


-- Decode a bit string into a string given a Huffman tree
-- Ignore extraneous bits at the end of the bit string
-- Return "" if either the bit string is "" or the tree is empty
decode :: String -> Tree -> String
decode "" t = ""
decode s Nil = ""
decode s t = dec s t
 where 
  dec ('0':s) (Node Nothing _ l _) = dec s l
  dec ('1':s) (Node Nothing _ _ r) = dec s r
  dec s (Node (Just char) _ _ _) = char:(dec s t)
  dec "" _ = ""

---------- Tests -------------------

testString = "eacaebeacaeaecbedeace"

testSortCharCounts =
  sortCharCounts [] == [] &&
  sortCharCounts [('d',4),('c',3),('b',2),('a',1)] == [('a',1),('b',2),('c',3),('d',4)] &&
  sortCharCounts [('b',2),('a',1),('d',4),('c',3)] == [('a',1),('b',2),('c',3),('d',4)] &&
  sortCharCounts [('a',1),('a',1),('a',1),('a',1)] == [('a',1),('a',1),('a',1),('a',1)]

testCountChars =
  countChars "" == [] &&
  sortCharCounts (countChars testString) == [('d',1),('b',2),('c',4),('a',6),('e',8)]

testMakeTreeList =
  makeTreeList [] == [] &&
  makeTreeList [('b',2),('a',1),('d',4),('c',3)]
    == [Node (Just 'b') 2 Nil Nil, Node (Just 'a') 1 Nil Nil,
        Node (Just 'd') 4 Nil Nil, Node (Just 'c') 3 Nil Nil]

testMakeOrderedTreeList =
  makeOrderedTreeList [] == [] &&
  makeOrderedTreeList [('b',2),('a',1),('d',4),('c',3)]
    == [Node (Just 'a') 1 Nil Nil, Node (Just 'b') 2 Nil Nil,
        Node (Just 'c') 3 Nil Nil, Node (Just 'd') 4 Nil Nil]

testTreeListInsert =
  treeListInsert (Node Nothing 3 Nil Nil) [] == [Node Nothing 3 Nil Nil] &&
  treeListInsert (Node Nothing 3 Nil Nil) (makeOrderedTreeList [('b',2),('a',1),('d',5),('c',4)])
    == [Node (Just 'a') 1 Nil Nil, Node (Just 'b') 2 Nil Nil,
        Node Nothing 3 Nil Nil,
        Node (Just 'c') 4 Nil Nil, Node (Just 'd') 5 Nil Nil]

testBuildTreeFromList =
  buildTreeFromList [] == Nil &&
  buildTreeFromList [(Node Nothing 5 Nil Nil)] == (Node Nothing 5 Nil Nil) &&
  buildTreeFromList [(Node (Just 'a') 1 Nil Nil),(Node (Just 'b') 2 Nil Nil)]
    == (Node Nothing 3 (Node (Just 'a') 1 Nil Nil) (Node (Just 'b') 2 Nil Nil))

testMakeHuffmanTree =
  makeHuffmanTree testString
    == Node Nothing 21
         (Node (Just 'e') 8 Nil Nil)
         (Node Nothing 13
           (Node (Just 'a') 6 Nil Nil)
           (Node Nothing 7
             (Node Nothing 3
               (Node (Just 'd') 1 Nil Nil)
               (Node (Just 'b') 2 Nil Nil))
           (Node (Just 'c') 4 Nil Nil)))

testMakeHuffmanMap =
  Map.lookup 'a' table == Just "10" &&
  Map.lookup 'b' table == Just "1101" &&
  Map.lookup 'c' table == Just "111" &&
  Map.lookup 'd' table == Just "1100" &&
  Map.lookup 'e' table == Just "0"
  where table = makeHuffmanTable testString

testEncode =
  (encode "ab" == "01" || encode "ab" == "10")  &&
  encode testString == "01011110011010101111001001111101011000101110"

testDecode =
  decode "01010" Nil == "" &&
  decode "" testTree == "" &&
  decode "1" testTree == "" &&
  decode "0" testTree == "e" &&
  decode (encode testString) testTree == testString &&
  decode "1101101100110101100" testTree == "badbed"
  where testTree = makeHuffmanTree testString

test =
  testCountChars &&
  testSortCharCounts &&
  testMakeTreeList &&
  testMakeOrderedTreeList &&
  testTreeListInsert &&
  testMakeHuffmanTree &&
  testMakeHuffmanMap &&
  testEncode &&
  testDecode

--------------------------------------------------------------------------

main =
  do 
    let result = "countChars: " ++ (show testCountChars) ++ "\n" ++
                 "sortCharCounts: " ++ (show testSortCharCounts) ++ "\n" ++
                 "makeTreeList: " ++ (show testMakeTreeList) ++ "\n" ++
                 "makeOrderedTreeList: " ++ (show testMakeOrderedTreeList) ++ "\n" ++
                 "treeListInsert: " ++ (show testTreeListInsert) ++ "\n" ++
                 "buildTreeFromList: " ++ (show testBuildTreeFromList) ++ "\n" ++
                 "makeHuffmanTree: " ++ (show testMakeHuffmanTree) ++ "\n" ++
                 "makeHuffmanMap: " ++ (show testMakeHuffmanMap) ++ "\n" ++
                 "encode: " ++ (show testEncode) ++ "\n" ++
                 "decode: " ++ (show testDecode)
    putStrLn result
