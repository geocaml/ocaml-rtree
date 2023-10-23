open Rtree
open Printf
(* Sample Point of Interest data structure *)
type poi = {
  name : string;
  latitude : float;
  longitude : float;
}


module Rtree =  struct

(* Function to insert POIs into the R-tree *)
let insert_pois rtree pois =
  List.iter (fun poi -> Rtree.insert rtree (poi.latitude, poi.longitude, poi)) pois

(* Function to load geospatial data from a CSV file *)
let load_geospatial_data_from_csv filename =
  let ic = open_in filename in
  let csv = Csv.of_channel ~has_header:true ic in
  let pois = ref [] in
  Csv.iter
    (fun row ->
      let name = row.(0) in
      let latitude = float_of_string row.(1) in
      let longitude = float_of_string row.(2) in
      pois := { name; latitude; longitude } :: !pois
    )
    csv;
  close_in ic;
  List.rev !pois

(* Example usage with geospatial data from a CSV file *)
let () =
  let rtree = Rtree.create () in

  (* Load geospatial data from external csv file *)
  let csv =
    List.map (fun name -> name, Csv.load name)
             [ "example/pois.csv"]
  

  (* Perform real-world spatial queries *)
  let nearby_pois = find_nearby_pois rtree 42.3601 (-71.0589) 1.0 in
  List.iter (fun poi -> Printf.printf("Nearby: %s\n" poi.name)) nearby_pois
