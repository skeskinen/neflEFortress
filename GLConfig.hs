module GLConfig where

data Atlas = White | Stairs | Ground | Wall | Empty | Item | Building |
                QuestionMark | Black | Focus | Creature1 | Creature2 |
                Creature3 | Shoe | Bag

atlas :: Atlas -> (Double, Double)
atlas White         = (1,1)
atlas Stairs        = (2,1)
atlas Ground        = (3,1)
atlas Wall          = (4,1)
atlas Empty         = (5,1)
atlas Item          = (6,1)
atlas Building      = (7,1)
atlas QuestionMark  = (8,1)
atlas Black         = (9,1)
atlas Focus         = (10,1)
atlas Creature1     = (1,2)
atlas Creature2     = (2,2)
atlas Creature3     = (3,2)
atlas Shoe          = (1,3)
atlas Bag           = (2,3)
