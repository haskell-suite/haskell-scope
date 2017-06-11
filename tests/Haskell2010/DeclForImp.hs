module DeclForImp where

foreign import stdcall          void1 :: ()
foreign import ccall            void2 :: ()
foreign import cplusplus        void3 :: ()
foreign import jvm              void4 :: ()
foreign import dotnet           void5 :: ()

foreign import ccall unsafe     void6 :: ()
foreign import ccall safe       void7 :: ()
foreign import ccall threadsafe void8 :: ()

foreign import ccall "void9"    void9 :: ()

collection = [void1,void2,void3,void4,void5
             ,void6,void7,void8,void9]
