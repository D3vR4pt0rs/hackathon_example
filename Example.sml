fun main () =
				(* Init contract *)
				let
        	val storage = [];
        	val customer = Context 1337 10217 storage;
        	val init_params = [ SCInt 1337, SCString "aviacompany", SCInt 1338, SCString "fuelcompany", SCString "This very simple agreement", SCInt 1339 ];
        	val result_init = Runtime.call 1 customer init_params;

				(* Get Agreement *)
					val supplier = Context 1338 10218 (Option.valOf(get (result_init)));
					val result_getting_agreement = Runtime.call 2 supplier [];

				(* Reject Agreement *)
					val supplier = Context 1338 10219 (Option.valOf(get (result_init)));
					val result_reject_agreement = Runtime.call 3 supplier [];

				(* Change Details of Agreement *)
					val customer = Context 1337 10220 (Option.valOf(get (result_reject_agreement)));
					val change_agreement_params = [SCString "Solve diffur", SCInt 1222];
					val result_change_agreement = Runtime.call 5 customer change_agreement_params;

				(* Approve Agreement *)
					val supplier = Context 1338 10221 (Option.valOf(get (result_change_agreement)));
					val result_approve_agreement = Runtime.call 4 supplier [];

				(* Create Price Change *)
					val supplier = Context 1338 10222 (Option.valOf(get (result_approve_agreement)));
					val pricechange_params = [SCInt 10000 , SCNegotiation WaitingCustomer,SCInt 60 ];
					val result_create_pricechange = Runtime.call 11 supplier pricechange_params;

				(* Get Price Change *)
					val customer = Context 1337 10223 (Option.valOf(get (result_create_pricechange)));
					val result_get_pricechange = Runtime.call 6 customer [SCInt 0];

				(* Get Number of PriceChanges *)
					val customer = Context 1337 10224 (Option.valOf(get (result_create_pricechange)));
					val result_get_number_price_change = Runtime.call 7 customer [];

				(* Reject PriceChange *)
					val customer = Context 1337 10225 (Option.valOf(get (result_create_pricechange)));
					val result_reject_price_change = Runtime.call 8 customer [];

				(* Create Price Change *)
				val supplier = Context 1338 10226 (Option.valOf(get (result_reject_price_change)));
				val pricechange_params = [SCInt 1000 , SCNegotiation WaitingCustomer,SCInt 60 ];
				val result_create_pricechange = Runtime.call 11 supplier pricechange_params;

				(* Decline Price Change
				val customer = Context 1337 10227 (Option.valOf(get (result_create_pricechange)));
				val result_decline_pricechange = Runtime.call 10 customer pricechange_params; *)

				(* Approve Price Change *)
				val customer = Context 1337 10225 (Option.valOf(get (result_create_pricechange)));
				val result_approve_price_change = Runtime.call 9 customer [];

				(* Add Task *)

				in
					(* if Option.isNone (get_err result_getting_agreement) then
							print(agreement_toPrettyString ((Option.valOf( scValue_to_Agreement( decodeValue(Option.valOf(get_ret_val ( result_getting_agreement ))))))))
					else
							print(Option.valOf(get_err (result_getting_agreement))) *)

					if Option.isNone (get_err result_create_pricechange) then
	             print(campaign_toPrettyString ((Option.valOf( scValue_to_campaign( decodeValue(Option.valOf(get ( result_create_pricechange ))))))))
	         else
	             print(Option.valOf(get_err ( result_create_pricechange )))

					 (* if Option.isNone (get_err result_get_pricechange) then
						   print(priceChange_toPrettyString ((Option.valOf( scValue_to_PriceChange ( decodeValue(Option.valOf(get_ret_val (result_get_pricechange ))))))))
						else
	  	         print(Option.valOf(get_err ( result_get_pricechange ))) *)

					(* if Option.isNone (get_err result_get_number_price_change) then
              print(Int.toString  (Option.valOf( scvalue_to_int (decodeValue(Option.valOf(get_ret_val (result_get_number_price_change )))))))
					else
					    print(Option.valOf(get_err (result_get_number_price_change ))) *)
					end;

main ();
