data Log a = Log [String] a

returnLog :: a -> Log a
returnLog = Log []
