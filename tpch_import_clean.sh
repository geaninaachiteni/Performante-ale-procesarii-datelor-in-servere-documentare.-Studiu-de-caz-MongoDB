#!/bin/bash
DB="tpch0_01__schema_431"
BASE="/c/Users/Geanina/TPCH_clean"

mongoimport --db $DB --collection lineitem_orders --file "$BASE/tpch0_01__schema_431___lineitem_orders.json" --drop
mongoimport --db $DB --collection customer --file "$BASE/tpch0_01__schema_431___customer.json" --drop
mongoimport --db $DB --collection nation_region --file "$BASE/tpch0_01__schema_431___nation_region.json" --drop
mongoimport --db $DB --collection lineitem --file "$BASE/tpch0_01__schema_431___lineitem.json" --drop
mongoimport --db $DB --collection partsupp_part --file "$BASE/tpch0_01__schema_431___partsupp_part.json" --drop
mongoimport --db $DB --collection partsupp --file "$BASE/tpch0_01__schema_431___partsupp.json" --drop
mongoimport --db $DB --collection supplier --file "$BASE/tpch0_01__schema_431___supplierb.json" --drop

