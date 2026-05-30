# Part 2

## STM 
`runReaderT` = strips the ReaderT layer → ExceptT Response STM a
`runExceptT` = strips the ExceptT layer → STM (Either Response a)
`atomically` = executes the STM transaction in IO → IO (Either Response a)


