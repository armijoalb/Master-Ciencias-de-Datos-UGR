resultado=db.runCommand({
    aggregate: "Cities",
    pipeline : [
    {$match: {CountryID: {$ne:254}}},    
    {$group: { _id: "$CountryID", "ciudad1":{$push: { citID: "$CityId",nomcity:"$City",lat: "$Latitude",lon:"$Longitude" }},
    "ciudad2":{$push: { citID: "$CityId",nomcity:"$City",lat: "$Latitude",lon:"$Longitude" }} }},
    {$unwind: "$ciudad1"},
    {$unwind: "$ciudad2"},
    {$project: {_id: 0, Pais: "$_id", ciudad1: "$ciudad1.nomcity", ciudad2: "$ciudad2.nomcity", 
        distancia:{ $sqrt: { $sum: [{$pow: [{ $subtract: [ "$ciudad1.lat","$ciudad2.lat" ]},2 ]} ,
     {$pow: [{ $subtract: [ "$ciudad1.lon","$ciudad2.lon" ]},2 ]} ]} } }} ,
   {$redact: {"$cond": [{$and: [{ "$lt": [ "$ciudad1", "$ciudad2" ]},{"$ne":["$distancia",0.0]}] },"$$KEEP","$$PRUNE"]}},
   {$group: { _id: "$Pais", "dist_min": {$min: "$distancia"} , "parejas":{$push: { ciudad1: "$ciudad1",ciudad2:"$ciudad2",distancia: "$distancia" }}}},
   {$unwind: "$parejas"},
   {$redact: {"$cond": [{ "$eq": [ "$dist_min", "$parejas.distancia" ] },"$$KEEP","$$PRUNE"]}},
   {$project: {_id: 0, "CountryID": "$_id", "Ciudad1": "$parejas.ciudad1", "Ciudad2": "$parejas.ciudad2", "distancia": "$dist_min" }}
    ],
   allowDiskUse: true,                          
   cursor: { batchSize: 180 } 
});