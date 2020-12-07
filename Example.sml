structure SupplierActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 2 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* getAgreement *)
	| 2 => Runtime.call 3 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* rejectAgreement *)
	| 3 => Runtime.call 4 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* approveAgreement *)
	| 4 => Runtime.call 11 (Context addr blockNum (Option.valOf(get (campaign)))) params (* createPriceChange *) (* SCInt price, SCNegotiation negotiation, SCInt startTime *)
	| 5 => Runtime.call 12 (Context addr blockNum (Option.valOf(get (campaign)))) params (* getTask *) (* SCInt taskID *)
	| 6 => Runtime.call 13 (Context addr blockNum (Option.valOf(get (campaign)))) params (* approveTask *) (* SCInt taskID *)
	| 7 => Runtime.call 14 (Context addr blockNum (Option.valOf(get (campaign)))) params (* rejectTask *) (* SCInt taskID *)
	| n => NONE "The function doesn't exist";
end

structure CustomerActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 2 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* getAgreement *)
	| 2 => Runtime.call 5 (Context addr blockNum (Option.valOf(get (campaign)))) params (* changeAgreementDetails *) (* SCSring details, SCInt bankAddr *)
	| 3 => Runtime.call 6 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* getPriceChangeWithNumber *)
	| 4 => Runtime.call 7 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* getPriceChangesLength *)
	| 5 => Runtime.call 8 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* rejectPrice *)
	| 6 => Runtime.call 9 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* approvePrice *)
	| 7 => Runtime.call 10 (Context addr blockNum (Option.valOf(get (campaign)))) [] (* declinePrice *)
	| 8 => Runtime.call 12 (Context addr blockNum (Option.valOf(get (campaign)))) params (* getTask *) (* SCInt taskID *)
	| 9 => Runtime.call 16 (Context addr blockNum (Option.valOf(get (campaign)))) params (* removeTask *) (* SCInt taskID *)
	| 10 => Runtime.call 17 (Context addr blockNum (Option.valOf(get (campaign)))) params (* addTask *) (*
		addTask params:
		SCInt taskId, SCNegotiation negotiation,
		SCInt captain_addr, SCString captain_name,
		SCInt worker_addr, SCString worker_name,
		SCInt expectedGas, SCInt requestedGas, SCInt suppliedGas, SCInt totalGas,
		SCInt requestTime, SCInt suppliedTime, SCInt completionTime, SCInt paymentTime,
		SCTaskStatus taskStatus, SCPaymentType paymentType
		*)
	| n => NONE "The function doesn't exist";
end

structure WorkerActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 18 (Context addr blockNum (Option.valOf(get (campaign)))) params (* readyToPerformTask *) (* SCInt taskID *)
	| 2  => Runtime.call 15 (Context addr blockNum (Option.valOf(get (campaign)))) params (* acceptTask *) (* SCInt taskID *)
	| 3 => Runtime.call 21 (Context addr blockNum (Option.valOf(get (campaign)))) params (* performTask *) (* SCInt taskID *)
	| 4 => Runtime.call 22 (Context addr blockNum (Option.valOf(get (campaign)))) params (* taskCompleted *) (* SCInt taskID, SCInt suppliedGas *)
	| n => NONE "The function doesn't exist";
end

structure CaptainActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 19 (Context addr blockNum (Option.valOf(get (campaign)))) params (* requestGas *) (* SCInt taskId, SCInt amount, SCInt paymentTime*)
	| 2 => Runtime.call 23 (Context addr blockNum (Option.valOf(get (campaign)))) params (* confirmTask *) (* SCInt taskID *)
	| n => NONE "The function doesn't exist";
end

structure BankActions =
struct
	fun make_action f addr blockNum campaign params =
	case f of
	1 => Runtime.call 20 (Context addr blockNum (Option.valOf(get (campaign)))) params (* paymentCompleted *) (* SCInt paymentId *)
	| n => NONE "The function doesn't exist";
end

structure Role =
struct
	fun choose_role f addr blockNum action campaign params =
	case f of
	1 => CustomerActions.make_action action  addr blockNum  campaign params
	| 2 => SupplierActions.make_action action addr blockNum  campaign params
	| 3 => WorkerActions.make_action action addr blockNum  campaign params
	| 4 => CaptainActions.make_action action addr blockNum  campaign params
	| 5 => BankActions.make_action action addr blockNum campaign params
	| n => NONE "The function doesn't exist";
end

fun reformat_campaign_to_string value = campaign_toPrettyString ((Option.valOf( scValue_to_campaign( decodeValue(Option.valOf(get ( value )))))))
fun reformat_agreement_to_string value = agreement_toPrettyString ((Option.valOf( scValue_to_Agreement( decodeValue(Option.valOf(get_ret_val ( value )))))))
fun reformat_pricechange_to_string value = priceChange_toPrettyString ((Option.valOf( scValue_to_PriceChange ( decodeValue(Option.valOf(get_ret_val ( value )))))))
fun reformat_task_to_string value = task_toPrettyString ((Option.valOf( scValue_to_Task ( decodeValue(Option.valOf(get_ret_val ( value )))))))


fun main () =
				(* Init contract *)
				let
        	val storage = [];
        	val customer = Context 1337 10217 storage;
        	val init_params = [ SCInt 1337, SCString "aviacompany", SCInt 1338, SCString "fuelcompany", SCString "This very simple agreement", SCInt 1339 ];
        	val result_init = Runtime.call 1 customer init_params;

					val result_getting_agreement_by_cus = Role.choose_role 1 1337 10218 1 result_init [];
					val _ = print("\nGet Agreement by Customer:\n");
					val _ = print ( reformat_agreement_to_string result_getting_agreement_by_cus)

					val result_getting_agreement_by_sup = Role.choose_role 2 1338 10219 1 result_init [];
					val _ = print("\nGet Agreement by Supplier:\n");
					val _ = print ( reformat_agreement_to_string result_getting_agreement_by_sup)


					val result_reject_agreement = Role.choose_role 2 1338 10220 2 result_init [];
					val _ = print("\nReject Agreement:\n");
					val _ = print (reformat_campaign_to_string result_reject_agreement)

					val result_change_agreement_details = Role.choose_role 1 1337 10221 2 result_reject_agreement [SCString "Buy all hamburgers", SCInt 1339];
					val _ = print("\nChange Agreement Details:\n");
					val _ = print (reformat_campaign_to_string result_change_agreement_details)

					val result_approve_agreement = Role.choose_role 2 1338 10222 3 result_change_agreement_details [];
					val _ = print("\nApprove Agreement:\n");
					val _ = print (reformat_campaign_to_string result_approve_agreement)

					val price_params = [SCInt 228, SCNegotiation WaitingCustomer, SCInt 666]
					val result_create_price_change = Role.choose_role 2 1338 10223 4 result_approve_agreement price_params;
					val _ = print("\nCreating Price Change:\n");
					val _ = print (reformat_campaign_to_string result_create_price_change);

					val result_get_price_changes_length = Role.choose_role 1 1337 10224 4 result_create_price_change [];
					(* val result_reject_price_change = Role.choose_role 1 1337 10225 5 result_create_price_change []; *)

					val result_approve_price_change = Role.choose_role 1 1337 10225 6 result_create_price_change [];
					val _ = print("\nApprove Price Change:\n");
					val _ = print (reformat_campaign_to_string result_approve_price_change);
					(* val result_decline_price_change = Role.choose_role 1 1337 10225 7 result_create_price_change []; *)

					(* Add Task *)
					val add_task_params = [SCInt 228, SCNegotiation WaitingSupplier,
					SCInt 1340, SCString "Aero",
					SCInt 1341, SCString "Oily",
					SCInt 1000, SCInt 1000, SCInt 1000, SCInt 1000,
					SCInt 1, SCInt 10, SCInt 11, SCInt 5,
					SCTaskStatus TaskNotAccepted, SCPaymentType Delayed];
					val result_task_add = Role.choose_role 1 1337 10226 10 result_approve_price_change add_task_params;
					val _ = print("\nAdding Task:\n");
					val _ = print(reformat_campaign_to_string result_task_add);

					val result_getting_tasks_by_sup = Role.choose_role 2 1338 10227 5 result_task_add [SCInt 228];
					val _ = print("\nGetting Task by Supplier:\n");
					val _ = print(reformat_task_to_string result_getting_tasks_by_sup);

					val result_approve_task = Role.choose_role 2 1338 10228 6 result_task_add [SCInt 228];
					val _ = print("\nApprove Task:\n");
					val _ = print(reformat_campaign_to_string result_approve_task);
					(* val result_reject_task = Role.choose_role 2 1338 10228 result_getting_tasks_by_sup 228;*)

					(*BLOCK 2*)

					val result_worker_accept_task = Role.choose_role 3 1341 10229 2 result_approve_task [SCInt 228];
					val _ = print("\nAccept Task from Worker:\n");
					val _ = print(reformat_campaign_to_string result_worker_accept_task);

					val result_worker_ready_to_perform_task = Role.choose_role 3 1341 10230 1 result_worker_accept_task [SCInt 228];
					val _ = print("\nWorker Ready to Perform Task:\n");
					val _ = print(reformat_campaign_to_string result_worker_ready_to_perform_task);

					val result_capitan_request_gas = Role.choose_role 4 1340 10231 1 result_worker_ready_to_perform_task [SCInt 228, SCInt 1000, SCInt 5];
					val _ = print("\nCapitan Request Gas:\n");
					val _ = print(reformat_campaign_to_string result_capitan_request_gas);

					val result_worker_perform_task = Role.choose_role 3 1341 10232 3 result_capitan_request_gas [SCInt 228];
					val _ = print("\nPerform Task by Worker:\n");
					val _ = print(reformat_campaign_to_string result_worker_perform_task);

					val result_worker_task_complete = Role.choose_role 3 1341 10233 4 result_worker_perform_task [SCInt 228, SCInt 1000];
					val _ = print("\nWorker Task Complete:\n");
					val _ = print(reformat_campaign_to_string result_worker_task_complete);

					val result_capitan_confirm_task = Role.choose_role 4 1340 10234 2 result_worker_task_complete [SCInt 228];
					val _ = print("\nonfirm Task by Captain:\n");
					val _ = print(reformat_campaign_to_string result_capitan_confirm_task);

					val result_payment_completed = Role.choose_role 5 1339 10235 1 result_capitan_confirm_task [SCInt 0];


				in
				if Option.isNone (get_err result_payment_completed) then
						print(campaign_toPrettyString ((Option.valOf( scValue_to_campaign( decodeValue(Option.valOf(get ( result_payment_completed ))))))))
				 else
						 print(Option.valOf(get_err ( result_payment_completed )))
				end;

main ();
