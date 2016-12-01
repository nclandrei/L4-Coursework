{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel


main :: IO ()
main = do
    personsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeList 
    case personsList of
        Just list -> do
                        print list
                        let l = list !! 0
                        personsNumbers <- scrapeURL (getURLs (l !! 1)) scrapePerson
                        print personsNumbers
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
    return (personName, url)

scrapePerson :: Scraper String [String]
scrapePerson = 
    chroots ("div" @: ["id" @= "sp_contactInfo"]) scrapeNumber

scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

getURLs :: (String, String) -> String
getURLs (_, url) = ("http://www.gla.ac.uk" ++ url)
