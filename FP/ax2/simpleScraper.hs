{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Text.LaTeX
import Data.Function
import Data.List
import Data.Maybe

main :: IO ()
main = do
    researchPersonsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeResearchList
    managementPersonsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeManagementList
    affiliatePersonsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeAffiliateList
    honoraryPersonsList <- scrapeURL "http://www.gla.ac.uk/schools/computing/staff/" scrapeHonoraryList
    let validResearchPersonsList = fromMaybe [] researchPersonsList
    let validManagementPersonsList = fromMaybe [] managementPersonsList
    let validAffiliatePersonsList = fromMaybe [] affiliatePersonsList
    let validHonoraryPersonsList = fromMaybe [] honoraryPersonsList
    let fullValidList = (validResearchPersonsList !! 0) ++ (validManagementPersonsList !! 0) ++ (validAffiliatePersonsList !! 0) ++ (validHonoraryPersonsList !! 0)
    let l = sortBy compare fullValidList
    let urls = map (\(x,y) -> y) l
    let names = map (\(x,y) -> x) l
    let contacts = map scrapeURLs l
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

scrapeURLs (x,y) = do
    phoneNumber <- scrapeURL y scrapePerson
    let validPhoneNumber = fromMaybe [""] phoneNumber
    return (x, validPhoneNumber)
