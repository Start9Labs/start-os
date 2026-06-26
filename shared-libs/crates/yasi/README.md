# yasi: Yet Another String Interner

Uses Arc'd references, and performs Hash and Eq using Display, so that you can
intern anything implementing Display without allocating unless the value doesn't
already exist
