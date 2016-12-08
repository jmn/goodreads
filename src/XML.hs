{-# LANGUAGE OverloadedStrings #-}
module XML where
import Types (Book(..))
--import Text.XML.Lens (Document, Element, (^?), (./), (^..), (??), root,  el, text, lengthOf)
import Text.XML.Lens
import Text.XML
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Monoid ((<>))
--import Control.Lens
import Control.Applicative

numReviews :: Document -> Int
numReviews doc = lengthOf ?? doc $ root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"

-- tities :: Document -> [DI.Text]
tities :: Document -> [Text]
tities doc = doc ^.. root . el "GoodreadsResponse" ./ el "reviews" ./ el "review" ./ el "book" ./ el "title" . text
-----------------------------
parseBookInfo :: Document -> Maybe Text --Either String String
parseBookInfo doc =
--    let book = doc ^? root . el "Goodreadsresponse" ./ el "book"
    doc ^? root . el "GoodreadsResponse" ./ el "book"   ./ el "description" . text
--    let book = doc ^.. root . el "GoodreadsResponse" ./ el "book"
--        title = book ^? root .el "book" ./ el "title" . text
--        descr = book ./ el "book" ./ el "description" . text
--    in if null title
--      then Left $ "Unable to find book"
--      else Right title

parseBookSearch :: Document -> Either String [Book]
parseBookSearch doc =
    let bookElems = doc ^.. root . el "GoodreadsResponse" ./ el "search" ./ el "results" ./ el "work"
        books = catMaybes $ fmap parseFindBook bookElems

    in if null bookElems
      then Left $ "Unable to parse any items from " <> show doc
      else if (length bookElems) /= (length books)
           then Left $ "Unable to parse all items from " <> show bookElems
           else Right books

parseFindBook :: Element -> Maybe Book
parseFindBook e = Book
  <$> t "title"       
  <*> Just (t "id")

  where t n = e ^? el "work" ./ el "best_book" ./ el n . text

parseGoodreadsFeed :: Document -> Either String [Book]
parseGoodreadsFeed doc =
  let bookElems = doc ^.. root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"
      books = catMaybes $ fmap parseBook bookElems

  in if null bookElems
      then Left $ "Unable to parse any items from " <> show doc
      else if (length bookElems) /= (length books)
           then Left $ "Unable to parse all items from " <> show bookElems
           else Right books

parseBook :: Element -> Maybe Book
parseBook e = Book
  <$> t "title"
  <*> Just (t "id")
  
  where t n = e ^? el "review" ./ el "book" ./ el n . text

