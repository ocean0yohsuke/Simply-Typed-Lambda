{-# LANGUAGE BangPatterns #-}

module Util.LISP (
    LISP(..), 
) where

import MonadX.Applicative

import Prelude hiding (
    head, tail, last, init,
    length, map, filter, zipWith, zip,
    foldl, foldr, mapM, append)

import Data.Monoid

-- for debug
import Debug.Trace 


class (Eq l, Show l) => LISP l where
    -- minimal definition --
    car :: l -> l
    cdr :: l -> l
    cons :: l -> l -> l
    nil :: l
    isCell :: l -> Bool
    ------------------------

    isNil :: l -> Bool
    isNil l = l == nil

    caar :: l -> l
    caar = car . car
    cadr :: l -> l
    cadr = car . cdr
    cdar :: l -> l
    cdar = cdr . car
    cddr :: l -> l
    cddr = cdr . cdr

    caaar :: l -> l
    caaar = car . caar
    cdaar :: l -> l
    cdaar = cdr . caar
    cadar :: l -> l
    cadar = car . cdar
    caadr :: l -> l
    caadr = car . cadr
    caddr :: l -> l
    caddr = car . cddr
    cdadr :: l -> l
    cdadr = cdr . cadr
    cddar :: l -> l
    cddar = cdr . cdar
    cdddr :: l -> l
    cdddr = cdr . cddr

    --
    -- List 
    -- 

    fromList :: [l] -> l
    fromList []     = nil
    fromList (x:xs) = cons x (fromList xs)

    isList :: l -> Bool
    isList l 
        | isNil l   = True
        | isCell l  = isList (cdr l)
        | otherwise = False
   
    head :: l -> l
    head = car
    tail :: l -> l
    tail = cdr
    last :: l -> l
    last l | isNil l  = error $ "LISP.last: invalid structure detected: "++ show l
           | isCell l = let l' = cdr l
                        in if isNil l'
                           then car l
                           else last l'
    init :: l -> l
    init l | isNil l  = error $ "LISP.last: invalid structure detected: "++ show l
           | isCell l = let l' = cdr l
                        in if isNil l'
                           then nil
                           else cons (car l) (init l')

    --
    --  
    -- 

    append :: l -> l -> l
    append l1 l2
        | not (isNil l1 || isCell l1) = error $ "LISP.append: invalid structure detected: "++ show l1
        | not (isNil l2 || isCell l2) = error $ "LISP.append: invalid structure detected: "++ show l2
        | isNil l1  = l2
        | isNil l2  = l1
        | otherwise = cons (car l1) (append (cdr l1) l2) 

    zipWith :: (l -> l -> l) -> l -> l -> l
    zipWith f l1 l2 
        | not (isNil l1 || isCell l1) = error $ "LISP.zipWith: invalid structure detected: "++ show l1
        | not (isNil l2 || isCell l2) = error $ "LISP.zipWith: invalid structure detected: "++ show l2
        | isNil l1 || isNil l2 = nil 
        | otherwise            = cons (f (car l1) (car l2)) $ zipWith f (cdr l1) (cdr l2)

    zip :: l -> l -> l
    zip = zipWith (\l r -> cons l (cons r nil))

    --
    -- Higher order function
    -- 

    map :: (l -> l) -> l -> l
    map f l 
        | isNil l  = nil
        | isCell l = cons (f (car l)) (map f (cdr l))
        | otherwise = error $ "LISP.map: invalid structure detected: "++ show l

    foldr :: (l -> acc -> acc) -> acc -> l -> acc
    foldr f acc l
        | isNil l  = acc
        | isCell l = 
            let acc' = foldr f acc (cdr l) 
            in  f (car l) acc'
        | otherwise = error $ "LISP.foldr: invalid structure detected: "++ show l
    foldl :: (acc -> l -> acc) -> acc -> l -> acc
    foldl f acc l 
        | isNil l  = acc
        | isCell l = 
            let acc' = f acc (car l)
            in  foldl f acc' (cdr l)
        | otherwise = error $ "LISP.foldl: invalid structure detected: "++ show l

    foldrM :: (Applicative m, Monad m) => (l -> acc -> m acc) -> acc -> l -> m acc
    foldrM f acc l
        | isNil l  = (*:) acc
        | isCell l = do
            acc' <- foldrM f acc (cdr l) 
            f (car l) acc'
        | otherwise = error $ "LISP.foldrM: invalid structure detected: "++ show l
    foldlM :: (Applicative m, Monad m) => (acc -> l -> m acc) -> acc -> l -> m acc
    foldlM f acc l 
        | isNil l  = (*:) acc
        | isCell l = do
            acc' <- f acc (car l)
            foldlM f acc' (cdr l)
        | otherwise = error $ "LISP.foldlM: invalid structure detected: "++ show l

    mapM :: (Applicative m, Monad m) => (l -> m l) -> l -> m l
    mapM f l 
        | not (isList l) = error $ "LISP.mapM: invalid structure detected: "++ show l
        | otherwise      = foldlM (\acc x -> append acc |$> (cons |$> f x |* nil)) nil l

    forM :: (Applicative m, Monad m) => l -> (l -> m l) -> m l
    forM = flip mapM

    reduce :: (Monoid a) => (l -> a) -> l -> a
    reduce f l 
        | isCell l  = let a = car l
                          d = cdr l
                      in  mappend (reduce f a) (reduce f d)
        | otherwise = f l

    --
    -- Prelude
    -- 

    toList :: l -> [l]
    toList l 
        | not (isList l) = error $ "LISP.toList: invalid structure detected: "++ show l
        | otherwise      = foldr (:) [] l

    length :: l -> Int
    length l 
        | not (isList l) = error $ "LISP.length: invalid structure detected: "++ show l
        | otherwise      = foldl (\acc x -> acc + 1) 0 l

    filter :: (l -> Bool) -> l -> l
    filter pred l
        | not (isList l) = error $ "LISP.filter: invalid structure detected: "++ show l
        | otherwise      = foldr (\x acc -> if pred x
                                            then cons x acc
                                            else acc) nil l

    fringe :: l -> l
    fringe l
        | not (isNil l || isCell l) = error $ "LISP.fringe: invalid structure detected: "++ show l
        | isNil l                   = nil
        | not (isCell (car l))      = cons (car l) (fringe (cdr l))
        | otherwise                 = append (fringe (car l)) (fringe (cdr l))

    --
    -- Set
    --

    isElem :: l -> l -> Bool 
    isElem x set 
        | not (isNil set || isCell set) = error $ "LISP.isElem: invalid structure detected: "++ show set
        | isNil set                     = False
        | x == (car set)                = True
        | otherwise                     = isElem x (cdr set)

    adjoin :: l -> l -> l
    adjoin x set 
        | not (isNil set || isCell set) = error $ "LISP.adjoin: invalid structure detected: "++ show set
        | otherwise                     = if isElem x set 
                                          then set 
                                          else cons x set

    intersection :: l -> l -> l
    intersection set1 set2 
        | not (isNil set1 || isCell set2) = error $ "LISP.intersection: invalid structure detected: "++ show set1
        | not (isNil set1 || isCell set2) = error $ "LISP.intersection: invalid structure detected: "++ show set2
        | isNil set1 || isNil set2        = nil
        | isElem (car set1) set2          = cons (car set1) (intersection (cdr set1) set2)
        | otherwise                       = intersection (cdr set1) set2

    union :: l -> l -> l
    union set1 set2
        | not (isNil set1 || isCell set2) = error $ "LISP.union: invalid structure detected: "++ show set1
        | not (isNil set1 || isCell set2) = error $ "LISP.union: invalid structure detected: "++ show set2
        | isNil set1             = set2
        | isElem (car set1) set2 = union (cdr set1) set2
        | otherwise              = cons (car set1) (union (cdr set1) set2) 

    subsets :: l -> l
    subsets l
        | not (isNil l || isCell l) = error $ "LISP.subsets: invalid structure detected: "++ show l
        | isNil l                   = cons nil nil
        | otherwise                 = append rest $ map (\x -> cons (car l) x) rest
      where
        rest = subsets (cdr l)


