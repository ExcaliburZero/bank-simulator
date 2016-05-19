module Accounts where

newtype CustomerName   = CustomerName String
  deriving Show
newtype AccountNumber  = AccountNumber Int
  deriving Show
newtype AccountBalance = AccountBalance Double
  deriving Show

data BankAccount = BankAccount
  { customerName   :: CustomerName
  , accountNumber  :: AccountNumber
  , accountBalance :: AccountBalance
  } deriving (Show)

{-|
   Changes the balance of the given account by the given amount.
   
   Positive amounts increase the balance of the account, while negative amounts
   reduce the balance of the account.

   >>> let acc = BankAccount {customerName = CustomerName "John", accountNumber = AccountNumber 123456789, accountBalance = AccountBalance 100.0}
   >>> changeAccountBalance acc 50
   BankAccount {customerName = CustomerName "John", accountNumber = AccountNumber 123456789, accountBalance = AccountBalance 150.0}

   >>> let acc = BankAccount {customerName = CustomerName "John", accountNumber = AccountNumber 123456789, accountBalance = AccountBalance 100.0}
   >>> changeAccountBalance acc (-50)
   BankAccount {customerName = CustomerName "John", accountNumber = AccountNumber 123456789, accountBalance = AccountBalance 50.0}
 -}
changeAccountBalance :: BankAccount -> Double -> BankAccount
changeAccountBalance account change = changedAccount
  where currentBalance  = case (accountBalance account) of
                            AccountBalance bal -> bal
        newBalance      = AccountBalance $ currentBalance + change
        changedAccount  = BankAccount (customerName account) (accountNumber account) newBalance
