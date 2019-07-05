
class Query rep a where
    queryTree :: rep -> IO (RoseTree a)
    stepTree  :: rep -> IO (Step rep a)
    step      :: (Lift rep rep') => Step rep a -> Step rep' a
    stepBy    :: (Lift rep rep) => Step rep a -> (rep -> Bool) -> Step rep a
    query     :: Step rep a -> (a -> a) -> IO a

data Step rep a

class Lift rep1 rep2
