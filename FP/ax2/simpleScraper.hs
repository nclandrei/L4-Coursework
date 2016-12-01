{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX

main :: IO ()
main = do
    personsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeList 
    case personsList of
        Just list -> do
            print list
            let urls = map (\(x,y) -> y) (head list)
            let personNumbers = map scrapeURLs urls
            execLaTeXT simple >>= renderFile "directory.tex"
        Nothing -> print "Text could not be retrieved!"

scrapeList :: Scraper String [[(String, String)]]
scrapeList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

scrapeUl :: Scraper String [(String, String)]
scrapeUl =
    chroots "li" scrapeLi

scrapeLi :: Scraper String (String, String)
scrapeLi = do
    personName <- text $ "a"
    url <- attr "href" $ "a"
    let fullurl = "http://www.gla.ac.uk" ++ url
    let personNumber = scrapeURL fullurl scrapePerson
    return (personName, fullurl)

scrapePerson :: Scraper String [String]
scrapePerson = 
    chroots ("div" @: ["id" @= "sp_contactInfo"]) scrapeNumber

scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

scrapeURLs :: String -> IO (Maybe [String])
scrapeURLs x = scrapeURL x scrapePerson

simple :: Monad m => LaTeXT_ m
simple = do
 thePreamble
 document theBody

 -- Preamble with some basic info.
thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 documentclass [] article
 author "Andrei-Mihai Nicolae"
 title "Functional Programming -- Assessed Exercise 2"

-- Body with a section.
theBody :: Monad m => LaTeXT_ m
theBody = do
 maketitle
 section "Hello"
 "This is a simple example using the "
 hatex
 " library. "
 textbf "Enjoy!"
 " "
 -- This is how we nest commands.
 textbf (large "Yoohoo!")
