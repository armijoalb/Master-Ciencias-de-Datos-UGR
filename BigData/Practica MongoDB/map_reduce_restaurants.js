db.runCommand({ mapReduce: "restaurants",
query : { "grades.score" : {"$gt" : 5} },
map : function Map(){
    var key = this.address.zipcode;
    emit(key, {
		"data":
		[
			{
				"name" : this.name,
				"lat"  : this.address.coord[0],
                "lon"  : this.address.coord[1]
			}
		]
	});
},
reduce : function Reduce(key,values){
    var reduced = {"data":[]};
	for (var i in values) {
		var inter = values[i];
		for (var j in inter.data) {
			reduced.data.push(inter.data[j]);
		}
	}
	return reduced;
},
finalize: function Finalize(key,reduced){
    if (reduced.data.length == 1) {
		return { "message" : "Esta ciudad solo tiene un restaurante" };
	}
	var min_dist = 999999999999;
	var rest1 = { "name": "" };
	var rest2 = { "name": "" };
	var c1;
	var c2;
	var d;
	var restaurantes_y_distancias = {"info":[]};

	
	// calculamos la distancia entre cada par de restaurantes y la distancia mínima.
	for (var i in reduced.data) {
		for (var j in reduced.data) {
			if (i >= j) continue;
			c1 = reduced.data[i];
			c2 = reduced.data[j];
			d = (c1.lat-c2.lat)*(c1.lat-c2.lat)+(c1.lon-c2.lon)*(c1.lon-c2.lon);
			if (d < min_dist && d >= 0) {
				min_dist = d;
			}
			rest1 = c1;
			rest2 = c2;
			restaurantes_y_distancias.info.push({"restaurante1": rest1.name,
							"restaurante2" : rest2.name,
                                                            "d": d });
		}
	}

	// Nos quedamos solamente con los restaurante que tengan distancia mínima.
	var finalRest = {"parejas":[]};
        var ya_incluidos = {"nombres":[]};
        var n1,n2;
	for (var i in restaurantes_y_distancias.info ){
                n1 = restaurantes_y_distancias.info[i].restaurante1;
                n2 = restaurantes_y_distancias.info[i].restaurante2;
		if(restaurantes_y_distancias.info[i].d == min_dist && !ya_incluidos.nombres.includes(n1)
                    && !ya_incluidos.nombres.includes(n2) ){
			finalRest.parejas.push(restaurantes_y_distancias.info[i]);
                        ya_incluidos.nombres.push(n1);
                        ya_incluidos.nombres.push(n2);
		}
	}
 
    return {"restaurantes":finalRest.parejas,
     "Distancia Minima": Math.sqrt(min_dist),
      "Cuenta":reduced.data.length};
},

out : "rest_mapreduce"
});