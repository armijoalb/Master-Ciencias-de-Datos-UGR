resultado = db.runCommand({
    aggregate : "restaurants",
    pipeline : [
    {$unwind : "$grades" },
    // Nos quedamos solamente con los datos que tengan una valoracion mayor que 5.
    {$match : { "grades.score" : {$gt : 5}}},
    // Eliminamos elementos duplicados.
    {$group : { _id : "$name",
        "address" : {$first:"$address"}
        }
    },
    // Agrupamos por cuidad y guardamos dos listas con el nombre del restaurante y sus 
    // coordenadas, para despues calcular las distancias.
    {$group: { _id: "$address.zipcode", 
    "restaurante1":{$push: { nomcity :"$_id",
                     lat : { $arrayElemAt : ["$address.coord", 0 ] }
                    ,lon: { $arrayElemAt : [ "$address.coord", 1 ]} 
                            }
                    },
    "restaurante2":{$push: { nomcity :"$_id",
                     lat: { $arrayElemAt : ["$address.coord", 0 ] }
                    ,lon: {$arrayElemAt : [ "$address.coord", 1 ]}  }
                    },
     "cuenta" : {$sum : 1} 
            }
    },
    // Calculamos las distancias entre los resturantes.
    {$unwind: "$restaurante1"},
    {$unwind: "$restaurante2"},
    {$project: {_id: 0, Ciudad: "$_id", cuenta : 1,
         rest1: "$restaurante1.nomcity", rest2: "$restaurante2.nomcity", 
        distancia:{ $sqrt: { $sum: [
            {$pow: [{ $subtract: [ "$restaurante1.lat","$restaurante2.lat" ]},2 ]} ,
            {$pow: [{ $subtract: [ "$restaurante1.lon","$restaurante2.lon" ]},2 ]} ]} 
        } }
    },
    // Nos quedamos con aquellos restaurantes con nombres diferentes y que su distancia sea >= 0.
    {$redact: {"$cond": [{$and: [{ "$lt": [ "$rest1", "$rest2" ]},
                {"$gte":["$distancia",0.0]}] },"$$KEEP","$$PRUNE"]}
    },
    // Guardamos todas las parejas.
    {$group: { _id: "$Ciudad", "dist_min": {$min: "$distancia"} ,
         "parejas":{$push: { rest1: "$rest1",rest2:"$rest2",distancia: "$distancia",
        cuenta : "$cuenta" }}
        }
    },
    {$unwind: "$parejas"},
    // Nos quedamos solamente con las parejas que tengan distancia minima.
    {$redact: {"$cond": [{ "$eq": [ "$dist_min", "$parejas.distancia" ] },"$$KEEP","$$PRUNE"]}
    },
    // Sacamos todas las parejas de restaurantes y su ciudad.
    {$project: {_id: 0, "CityID": "$_id", "Restaurante1": "$parejas.rest1",
                 "Restaurante2": "$parejas.rest2", "distancia": "$dist_min",
                "cuenta":"$parejas.cuenta"}
    },
    // Los organizamos de la forma Ciudad : parejas de restaurantes y su distancia.
    {$group : { _id : "$CityID" , 
        "parejas": {$push : {"restaurante 1" : "$Restaurante1",
            "restaurante 2" : "$Restaurante2" , "distancia" : "$distancia" , "Cuenta" : "$cuenta" }
        }}
    },
    // Declaramos archivo de salida.
    {$out : "rest_aggregate"}
    ],
    allowDiskUse:true,
    cursor : {batchSize : 100000},
    
});