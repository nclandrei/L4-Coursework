{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import Control.Conditional

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
    let l = sortBy compare fullValidList
    let noDuplicatesList = removeDuplicates l
    let contacts = map scrapeContactURL noDuplicatesList
    contactsWithPhoneNumbers <- filterM (\(x, y) -> (ifM (y ==: []) (False) (True))) contacts
    mapM_ (\x -> (x>>=(print))) contacts

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

scrapeNumber :: Scraper String String
scrapeNumber = do
    num <- text $ "p"
    return num

scrapeContactURL (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    let validPhoneNumber = fromMaybe [""] phoneNumber
    return (x, validPhoneNumber)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

removePeopleWithoutNumbers (a, b) 
    | (null b) = False
    | otherwise = True

(==:) :: ( Eq a,Monad m) => m a -> m a -> m Bool
(==:) = liftM2 (==)
