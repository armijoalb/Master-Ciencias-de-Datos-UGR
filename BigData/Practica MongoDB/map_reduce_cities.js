db.runCommand({ mapReduce: "Cities",
 map : function Map() {
	var key = this.CountryID;
	emit(key, {
		"data":
		[
			{
				"name" : this.City,
				"lat"  : this.Latitude,
				"lon"  : this.Longitude
			}
		]
	});
},
 reduce : function Reduce(key, values) {
	var reduced = {"data":[]};
	for (var i in values) {
		var inter = values[i];
		for (var j in inter.data) {
			reduced.data.push(inter.data[j]);
		}
	}
	return reduced;
},
 finalize : function Finalize(key, reduced) {
	if (reduced.data.length == 1) {
		return { "message" : "Este pa�s s�lo tiene una ciudad" };
	}
	var min_dist = 999999999999;
	var city1 = { "name": "" };
	var city2 = { "name": "" };
	var c1;
	var c2;
	var d;
	for (var i in reduced.data) {
		for (var j in reduced.data) {
			if (i>=j) continue;
			c1 = reduced.data[i];
			c2 = reduced.data[j];
			d = (c1.lat-c2.lat)*(c1.lat-c2.lat)+(c1.lon-c2.lon)*(c1.lon-c2.lon);
			if (d < min_dist && d > 0) {
				min_dist = d;
				city1 = c1;
				city2 = c2;
			}
		}
	}
	return {"city1": city1.name, "city2": city2.name, "dist": Math.sqrt(min_dist),"Cuenta":reduced.data.length};
},
 query : { "CountryID" : { "$ne" : 254 } },
 out:  "rest_cities_mapReduce"
 });