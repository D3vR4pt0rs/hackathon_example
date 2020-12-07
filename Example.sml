structure SupplierActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 2 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 2 => Runtime.call 3 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 3 => Runtime.call 4 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 4 => Runtime.call 11 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 5 => Runtime.call 12 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 6 => Runtime.call 13 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 7 => Runtime.call 14 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 8 => Runtime.call 15 (Context addr blockNum (Option.valOf(get (campaign)))) params
end

structure CustomerActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 2 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 2 => Runtime.call 5 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 3 => Runtime.call 6 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 4 => Runtime.call 7 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 5 => Runtime.call 8 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 6 => Runtime.call 9 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 7 => Runtime.call 10 (Context addr blockNum (Option.valOf(get (campaign)))) []
	| 8 => Runtime.call 12 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 9 => Runtime.call 16 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 10 => Runtime.call 17 (Context addr blockNum (Option.valOf(get (campaign)))) params
end

structure WorkerActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 18 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 2 => Runtime.call 21 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 3 => Runtime.call 22 (Context addr blockNum (Option.valOf(get (campaign)))) params
end

structure CaptainActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 19 (Context addr blockNum (Option.valOf(get (campaign)))) params
	| 2 => Runtime.call 23 (Context addr blockNum (Option.valOf(get (campaign)))) params
end

structure BankActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 20 (Context addr blockNum (Option.valOf(get (campaign)))) params
end

structure Role =
struct
	fun choose_role f addr blockNum action params campaign =
	case f of
	1 => CustomerActions.make_action action  addr blockNum  params campaign
	| 2 => SupplierActions.make_action action addr blockNum  params campaign
	| 3 => WorkerActions.make_action action addr blockNum  params campaign
	| 4 => CaptainActions.make_action action addr blockNum  params campaign
	| 5 => BankActions.make_action action addr blockNum  params campaign
end

fun main () =
				(* Init contract *)
				let
        	val storage = [];
        	val customer = Context 1337 10217 storage;
        	val init_params = [ SCInt 1337, SCString "aviacompany", SCInt 1338, SCString "fuelcompany", SCString "This very simple agreement", SCInt 1339 ];
        	val result_init = Runtime.call 1 customer init_params;

					val result_getting_agreement = Role.choose_role 1 1337 10218 1 result_init [];


				(* Add Task *)

				in
					 if Option.isNone (get_err result_getting_agreement) then
							print(agreement_toPrettyString ((Option.valOf( scValue_to_Agreement( decodeValue(Option.valOf(get_ret_val ( result_getting_agreement ))))))))
					else
							print(Option.valOf(get_err (result_getting_agreement)))

					(* if Option.isNone (get_err result_create_pricechange) then
	             print(campaign_toPrettyString ((Option.valOf( scValue_to_campaign( decodeValue(Option.valOf(get ( result_create_pricechange ))))))))
	         else
	             print(Option.valOf(get_err ( result_create_pricechange ))) *)

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
