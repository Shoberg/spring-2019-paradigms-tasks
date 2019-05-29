{-|
  Структура данных "Data.Map.Strict" лежит в нашем классе типов 'Map',
  все функции отображаются один-к-одному.
-}
module MapInstance where
import Map
import qualified Data.Map.Strict as SMap

instance Map SMap.Map where
    empty = SMap.empty
    singleton = SMap.singleton
    fromList = SMap.fromList
    toAscList = SMap.toAscList
    insert = SMap.insert
    insertWith = SMap.insertWith
    insertWithKey = SMap.insertWithKey
    delete = SMap.delete
    adjust = SMap.adjust
    adjustWithKey = SMap.adjustWithKey
    update = SMap.update
    updateWithKey = SMap.updateWithKey
    alter = SMap.alter
    lookup = SMap.lookup
    member = SMap.member
    notMember = SMap.notMember
    null = SMap.null
    size = SMap.size
