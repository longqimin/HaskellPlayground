{-# LANGUAGE OverloadedStrings #-}

import Codec.Archive.Tar hiding (pack)
import Codec.Compression.GZip
import qualified Filesystem.Path as FP
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import qualified Data.ByteString.Lazy as BL
import System.Directory (getTemporaryDirectory, copyFile)

tar :: FP.FilePath -> FP.FilePath -> IO ()
tar src dest = do
	let pathPieces = FP.splitDirectories src
	let (base, fileOrDir) = if FP.filename src == FP.empty then (FP.concat $ init pathPieces, last pathPieces) 
													 else (FP.directory src, FP.filename src) 
	tmpDir <- getTemporaryDirectory
	let tmpFile = decodeString tmpDir FP.</> dest
	create (encodeString tmpFile) (encodeString base) $ map encodeString [fileOrDir]
	copyFile (encodeString tmpFile) (encodeString dest)

gzip :: FP.FilePath -> FP.FilePath -> IO ()
gzip src dest = do
	raw <- BL.readFile (encodeString src)
	let compressed = compress raw 
	BL.writeFile (encodeString dest) compressed

main :: IO ()
main = tar (decodeString ".") (decodeString "all.tar")