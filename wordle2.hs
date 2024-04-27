--  Jeu Wordle en Haskell (version 2)

import System.Random
import Data.List
import Data.Char


wordlistFile :: String
answerlistFile :: String

-- Mots en anglais
-- wordlist_en.txt contient les 12947 mots de 5 lettres (de la langue anglaise) acceptés par Wordle
-- answerlist.txt contient les 2309 mots de 5 lettres (de la langue anglaise) qui sont des réponses potentielles dans Wordle (il s'agit d'un sous-ensemble de wordlist.txt qui exclut les mots trop obscurs, les pluriels, etc.)
-- wordlist_en.txt et answerlist_en.txt sont tous deux tirés de https://dagshub.com/arjvik/wordle-wordlist
-- Veuillez décommenter les deux lignes ci-dessous pour avoir les mots en anglais

-- wordlistFile = "wordlist_en.txt"
-- answerlistFile = "answerlist_en.txt"
    
-- Mots en français
-- wordlist_fr.txt contient 6812 mots de 5 lettres (de la langue française)
-- answerlist_fr.txt contient 4452 mots de 5 lettres (de la langue anglaise) qui sont des réponses potentielles dans Wordle (il s'agit d'un sous-ensemble de mots_fr_accents.txt qui exclut les mots avec accents)
-- wordlist_fr.txt et answerlist_fr.txt sont tirés d'une liste des mots de la langue française (https://git.esiee.fr/demphita/tous-les-mots) auquel nous avons conservé les mots de 5 lettres
-- Veuillez commenter les deux lignes ci-dessous pour avoir les mots en anglais

wordlistFile = "wordlist_fr.txt"
answerlistFile = "answerlist_fr.txt"
    

-- Nombre de réponses possibles
maxGuesses :: Int
maxGuesses = 6

-- Affiche l'introduction au jeu
print_intro :: IO ()
print_intro = do
  putStrLn "\n"
  putStrLn ("Vous disposez de " ++ show maxGuesses ++ " tentatives pour deviner le mot.")
  putStrLn "Le mot est un mot de 5 lettres, choisi dans une liste de mots français."
  putStrLn "Saisissez une réponse après chaque question."
  putStrLn "Bonne chance !"

-- Charge une liste de mots à partir d'un fichier
load_wordlist :: String -> IO [String]
load_wordlist filename = do
  contents <- readFile filename --  lit le contenu du fichier spécifié par 'filename' et renvoie son contenu sous forme de chaîne de caractères
  return (lines contents) -- divise la chaîne de caractères 'contents' en une liste de chaînes de caractères à chaque retour à la ligne

-- Sélectionne de manière aléatoire un mot à partir d'une liste de mots
pick_solution :: [String] -> IO String
pick_solution wordlist = do
  index <- randomRIO (0, length wordlist - 1) -- génère un nombre aléatoire dans l'intervalle spécifié ('<-' est utilisé pour extraire ce nombre de l'action I/O et le lier à la variable 'index')
  return (wordlist !! index) --  renvoie le mot correspondant à l'indice aléatoire ('!!' est utilisé pour accéder à l'élément à un indice spécifique dans une liste)

-- Type de données qui représente l'état du jeu
data GameState = GameState
  { guesses :: [String] -- liste de chaînes de caractères qui contient les tentatives faites par le joueur
  , numGuesses :: Int -- nombre total de tentatives effectuées par le joueur
  , answer :: String --  le mot à deviner dans le jeu
  , wordlist :: [String] -- la liste de mots à partir de laquelle le mot à deviner est sélectionné
  }

-- Crée et renvoie l'état initial du jeu.
initialState :: [String] -> String -> GameState
initialState wordlist answer = GameState
  { guesses = [] -- initialise le champ 'guesses' de l'état du jeu comme une liste vide (aucune tentative n'a été faite initialement)
  , numGuesses = 0 -- initialise le champ 'numGuesses' à zéro (aucune tentative effectuée)
  , answer = answer -- initialise le champ 'answer' de l'état du jeu avec le mot à deviner fourni en entrée
  , wordlist = wordlist -- initialise le champ 'wordlist' avec la liste de mots fournie en entrée
  }

-- Prend une chaîne de caractères en entrée et la convertit en majuscules (pour un meilleur rendu dans le terminal)
toUpper :: String -> String
toUpper = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

-- Prend une chaîne de caractères en entrée et la convertit en minuscules (pour simplifier les tests de validité des tentatives)
toLower :: String -> String
toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- Vérifie la présence d'accents dans une chaîne de caractères
containsAccents :: String -> Bool
containsAccents = any isAccent

-- Vérifie si un caractère possède un accent
isAccent :: Char -> Bool
isAccent c = c `elem` "àáâãäèéêëìíîïòóôõöùúûüçÀÁÂÃÄÈÉÊËÌÍÎÏÒÓÔÕÖÙÚÛÜÇ"

-- Renvoie le feedback pour un caractère unique lors d'une tentative
returnFeedbackChar :: String -> String -> Char -> Int -> Char
returnFeedbackChar answer guess guessChar guessIndex
  | answer !! guessIndex == guessChar = '🟩' -- si le caractère de la réponse à l'indice 'guessIndex' correspond au caractère de la tentative 'guessChar', alors le caractère est correctement placé 
  | elem guessChar answer = '🟨' --  si le caractère de la tentative 'guessChar' est présent dans la réponse, mais pas à la bonne position, alors cela signifie qu'il est présent mais mal placé 
  | otherwise = '⬜' -- si aucune des conditions précédentes n'est remplie, cela signifie que le caractère de la tentative n'est pas présent dans la réponse

-- Renvoie le feedback pour une tentative complète (une chaîne de caractères) sous forme d'une liste de couleurs représentant le feedback pour chaque caractère dans la tentative
returnFeedbackWord :: String -> String -> String
returnFeedbackWord answer guess = zipWith (returnFeedbackChar answer guess) guess [0..] -- utilise la fonction 'zipWith' pour appliquer la fonction 'returnFeedbackChar' à chaque caractère de la tentative ('guess') et son indice correspondant, par rapport à la réponse ('answer')

-- Renvoie une tentative avec son feedback concaténé
returnGuess :: String -> String -> String
returnGuess answer guess = returnFeedbackWord answer guess ++ " : " ++ Main.toUpper guess -- combine le feedback de la tentative avec la tentative elle-même, et renvoie le résultat

-- Renvoie toutes les tentatives avec leurs feedbacks concaténés
returnGuesses :: [String] -> String -> String
returnGuesses guesses answer = 
  let guessesString = intercalate "\n" (map (returnGuess answer) guesses) -- applique la fonction 'returnGuess' à chaque tentative de la liste 'guesses', générant ainsi une liste de chaînes de caractères contenant chaque tentative avec son feedback
      remainingGuesses = intercalate "\n" (replicate (maxGuesses - length guesses) (replicate 5 '⬛')) --  génère une chaîne de caractères représentant les tentatives restantes sous forme de blocs vides
  in if null guesses then remainingGuesses else guessesString ++ "\n" ++ remainingGuesses -- vérifie si la liste des tentatives est vide. Si c'est le cas, cela signifie qu'aucune tentative n'a été faite, donc la fonction renvoie uniquement les tentatives restantes

-- Définit un nouveau type de données, avec deux constructeurs de données : Error et Good
data Result = Error String | Good ()

-- Vérifie si une tentative est valide dans le contexte d'un état de jeu donné
checkGuess :: GameState -> String -> Result
checkGuess state guess
  | length guess /= 5 = Error "Le mot doit avoir une longueur de 5 caractères." -- vérifie si la tentative n'est pas exactement de 5 caractères
  | any isAccent guess = Error "Le mot ne peut pas contenir d'accents." -- vérifie si la tentative contient des accents
  | not (all (`elem` (['a'..'z'] ++ ['A'..'Z'])) guess) = Error "Le mot doit contenir seulement des lettres." -- vérifie si la tentative contient des caractères autres que des lettres
  | not (elem guess (wordlist state)) = Error "Le mot doit être présent dans la liste des mots acceptés." -- vérifie si la tentative n'est pas présente dans la liste des mots acceptés de l'état de jeu
  | elem guess (guesses state) = Error "Le mot ne doit pas déjà avoir été suggéré." -- vérifie si la tentative est déjà présente dans la liste des tentatives précédentes
  | otherwise = Good () -- sinon, la tentative est valide

-- Boucle principale du jeu
gameLoop :: GameState -> IO ()
gameLoop state = do
  putStrLn "\n"
  putStrLn ( "=========================" )
  putStrLn ( returnGuesses (reverse (guesses state)) (answer state) ) -- affiche toutes les tentatives effectuées jusqu'à présent avec leurs feedbacks
  putStrLn ( "Tentatives restantes : " ++ show (maxGuesses - numGuesses state) ++ "/" ++ show maxGuesses ) -- affiche le nombre de tentatives restantes sur le nombre total autorisé
  case () of -- vérifie diverses conditions de fin de jeu
    _ | not (null (guesses state)) && head (guesses state) == answer state -> do -- si une tentative a été faite et que la première tentative est égale à la réponse, alors le joueur a gagné
      putStrLn "\n"
      putStrLn "\tVOUS AVEZ GAGNÉ !"
      putStrLn "\n"
      putStrLn ( "Le mot à trouver était : " ++ Main.toUpper (answer state) ) -- si le nombre de tentatives effectuées atteint ou dépasse le nombre maximal autorisé 
      putStrLn ( "=========================" )
    _ | numGuesses state >= maxGuesses -> do
      putStrLn "\n"
      putStrLn "\tVOUS AVEZ PERDU !"
      putStrLn "\n"
      putStrLn ( "Le mot à trouver était : " ++ Main.toUpper (answer state) )
      putStrLn ( "=========================" )
    _ -> do -- sinon, le jeu continue. Le joueur est invité à effectuer une nouvelle tentative
      putStrLn "Faites une proposition : "
      guess <- getLine
      let guessLower = Main.toLower guess
      putStrLn ( "=========================" )
      case checkGuess state guessLower of -- vérifie si la tentative est valide en utilisant la fonction 'checkGuess'
        Error err -> do -- si la tentative est invalide, un message d'erreur est affiché, indiquant la raison de l'invalidité
          putStrLn "\n"
          putStrLn "Tentative erronée :"
          putStrLn err
          gameLoop state
        Good () -> do -- si la tentative est valide, une nouvelle configuration de l'état du jeu est créée avec la tentative ajoutée à la liste des tentatives et le nombre de tentatives mis à jour
          let state' = state { guesses = guessLower : guesses state, numGuesses = numGuesses state + 1 }
          gameLoop state'

-- Fonction main : entrée principale du programme
main :: IO ()
main = do
  print_intro -- affiche le message d'introduction
  wordlist <- load_wordlist wordlistFile -- charge une liste de mots à partir d'un fichier
  answerlist <- load_wordlist answerlistFile -- charge une liste de réponses possibles à partir d'un fichier
  answer <- pick_solution answerlist -- choisit un mot aléatoire dans la liste
  let state = initialState wordlist answer -- initialise l'état du jeu

  -- Affiche la solution (utilisé pour le débogage)
  -- putStrLn ( "DEBUG : Solution: " ++ Main.toUpper answer)

  gameLoop state -- démarrer la boucle du jeu
