{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX
import Text.Regex
import Text.Regex.Base
import Text.Regex.Posix
import Data.List
import Data.Function
import Data.List.Split
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
    let universityURL = "http://www.gla.ac.uk/schools/computing/staff/"
    researchPersonsList <- scrapeURL universityURL scrapeResearchList
    managementPersonsList <- scrapeURL universityURL scrapeManagementList
    affiliatePersonsList <- scrapeURL universityURL scrapeAffiliateList
    honoraryPersonsList <- scrapeURL universityURL scrapeHonoraryList
    let validResearchPersonsList = fromMaybe [] researchPersonsList
    let validManagementPersonsList = fromMaybe [] managementPersonsList
    let validAffiliatePersonsList = fromMaybe [] affiliatePersonsList
    let validHonoraryPersonsList = fromMaybe [] honoraryPersonsList
    let fullValidList = (validResearchPersonsList !! 0) ++ (validManagementPersonsList !! 0) ++ (validAffiliatePersonsList !! 0) ++ (validHonoraryPersonsList !! 0)
    let sortedAndNoDuplicatesList = removeDuplicates fullValidList
    contacts <- mapM scrapeContactURL sortedAndNoDuplicatesList
    let peopleWithoutEmptyInfo = filter removePeopleWithoutNumbers contacts
    let contactsWithHeadOfList = map removePhoneNumberList peopleWithoutEmptyInfo
    let contactsWithTelephone = filter (\(x,y) -> (isInfixOf "telephone" y)) contactsWithHeadOfList
    let finalContacts = map getPhoneNumberFromString contactsWithTelephone
    let records = finalContacts
    body <- execLaTeXT (constructDocument records)
    renderFile "directory.tex" body

scrapeResearchList :: Scraper String [[(String, String)]]
scrapeResearchList = 
    chroots ("div" @: ["id" @= "research-teaching"]) scrapeUl

scrapeManagementList :: Scraper String [[(String, String)]]
scrapeManagementList =
    chroots ("div" @: ["id" @= "management-support"]) scrapeUl

scrapeAffiliateList :: Scraper String [[(String, String)]]
scrapeAffiliateList =
    chroots ("div" @: ["id" @= "affiliate"]) scrapeUl

scrapeHonoraryList :: Scraper String [[(String, String)]]
scrapeHonoraryList =
    chroots ("div" @: ["id" @= "honorary-visiting"]) scrapeUl

scrapeUl :: Scraper String [(String, String)]
scrapeUl =
    chroots "li" scrapeLi

scrapeLi :: Scraper String (String, String)
scrapeLi = do
    personName <- text $ "a"
    url <- attr "href" $ "a"
    let fullurl = "http://www.gla.ac.uk" ++ url
    return (personName, fullurl)

scrapePerson :: Scraper String [String]
scrapePerson = 
    chroots ("div" @: ["id" @= "sp_contactInfo"]) scrapeNumber

scrapePersonTwo :: Scraper String String
scrapePersonTwo = do
    content <- text $ "p" @: ["style" @= "margin: 0 0 10px 25px; padding: 5px; color: ##333;"]
    return content

scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

scrapeContactURL (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    case phoneNumber of
        Just number -> do
            return (x, number)
        Nothing -> do
            print "am ajuns aici"
            alternativeNumber <- scrapeURL y scrapePersonTwo
            let validPhoneNumber = fromMaybe "" alternativeNumber
            return (x, [validPhoneNumber])
--    let validPhoneNumber = fromMaybe [""] phoneNumber
--    return (x, validPhoneNumber)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

removePeopleWithoutNumbers (a, b) 
    | (null b) = False
    | otherwise = True

removePhoneNumberList :: (String, [String]) -> (String, String)
removePhoneNumberList (x,y) = (x, (head y))

getPhoneNumberFromString :: (String, String) -> (String, String)
getPhoneNumberFromString (x,y)  = (x, (y =~ ("[ +()]*[0-9][ +()0-9]*" :: String)))

constructDocument :: Monad m => [(String, String)] -> LaTeXT_ m
constructDocument records = do
    documentclass [] article
    author "Andrei-Mihai Nicolae (2147392n)"
    title "Telephone Directory"
    document (constructDocumentBody records)

constructDocumentBody :: Monad m => [(String, String)] -> LaTeXT_ m
constructDocumentBody records = do
    maketitle
    bigskip
    center $ textbf "Records"
    mapM_ (\(name, phone) -> do
        textbf (fromString name)
        hfill
        fromString phone
        lnbk) records
