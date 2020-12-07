exception ErrorMsg string;

datatype 'a optionErr = SOME 'a | RET 'a 'a | NONE string; 

(* OptionErr -> ('a option) *)
fun get x = 
    case x of 
    SOME x => Some x
    | RET _ _ => None
    | NONE msg => (print msg; None);
(* OptionErr -> (string option) *)
fun get_err x =
    case x of 
    NONE x => Some x
    | RET _ _ => None
    | SOME _ => None;
(* OptionErr -> ('a option) *)
fun get_ret_state x = 
    case x of
    RET a _ => Some a 
    | NONE _ => None
    | SOME _ => None;
(* OptionErr -> ('a option) *)
fun get_ret_val x = 
    case x of
    RET _ a => Some a 
    | NONE _ => None
    | SOME _ => None;datatype Negotiation = NotSet | WaitingCustomer | WaitingSupplier | NegotiationRejected | NegotiationApproved ;
datatype PaymentStatus = WaitingForPayment | PaymentCompleted | PaymentRejected ;
datatype Phase = PhaseAgreement | PhaseTasks | PhaseDeclined ;
datatype TaskStatus = TaskNotAccepted | TaskAccepted | TaskReadyToPerform | GasRequested | Performing | Confirmed | TaskCompleted ;
datatype PaymentType = Pre | Post | Delayed ;

datatype Context = Context int int (Word8.word list);
fun get_context_msgSender (Context msgSender _ _) = msgSender;
fun get_context_blockNum (Context _ blockNum _) = blockNum;
fun get_context_storage (Context _ _ storage) = storage;

datatype Person = Person int string;
(*setters for Person*)
fun set_person_addr (Person a name) value = Person value name;
fun set_person_name (Person addr a) value = Person addr value;
(*getters for Person*)
fun get_person_addr (Person addr _) = addr;
fun get_person_name (Person _ name) = name;

datatype AgreementDetails = AgreementDetails string int;
(*setters for AgreementDetails*)
fun set_agreementDetails_details (AgreementDetails details a ) value = AgreementDetails value a ;
fun set_agreementDetails_bankAddress (AgreementDetails a bankAddress ) value = AgreementDetails a value ;
(*getters for AgreementDetails*)
fun get_agreementDetails_details (AgreementDetails details _ ) = details;
fun get_agreementDetails_bankAddress (AgreementDetails _ bankAddress ) = bankAddress;

datatype Agreement = Agreement Negotiation Person Person AgreementDetails;
(*setters for Agreement*)
fun set_agreement_negotiation (Agreement negotiation a b c) value = Agreement value a b c;
fun set_agreement_customer (Agreement a customer b c) value = Agreement a value b c;
fun set_agreement_supplier (Agreement a b supplier c) value = Agreement a b value c;
fun set_agreement_details (Agreement a b c details) value = Agreement a b c value;
(*getters for Agreement*)
fun get_agreement_negotiation (Agreement negotiation _ _ _) = negotiation;
fun get_agreement_customer (Agreement _ customer _ _) = customer;
fun get_agreement_supplier (Agreement _ _ supplier _) = supplier;
fun get_agreement_details (Agreement _ _ _ details) = details;

datatype PriceChange = PriceChange int Negotiation int;
(*setters for PriceChange*)
fun set_priceChange_price (PriceChange price a b) value = PriceChange value a b;
fun set_priceChange_negotiation (PriceChange a negotiation b) value = PriceChange a value b;
fun set_priceChange_startTime (PriceChange a b startTime) value = PriceChange a b value;
(*getters for PriceChange*)
fun get_priceChange_price (PriceChange price _ _) = price;
fun get_priceChange_negotiation (PriceChange _ negotiation _) = negotiation;
fun get_priceChange_startTime (PriceChange _ _ startTime) = startTime;

datatype PaymentOrder = PaymentOrder int int int int PaymentStatus bool;
(*setters for PaymentOrder*)
fun set_PaymentOrder_amount (PaymentOrder amount a b c d e) value = PaymentOrder value a b c d e;
fun set_PaymentOrder_paymentTime (PaymentOrder a paymentTime b c d e) value = PaymentOrder a value b c d e;
fun set_PaymentOrder_paymentId (PaymentOrder a b paymentId c d e) value = PaymentOrder a b value c d e;
fun set_PaymentOrder_taskId (PaymentOrder a b c taskId d e) value = PaymentOrder a b c value d e;
fun set_PaymentOrder_paymentStatus (PaymentOrder a b c d paymentStatus e) value = PaymentOrder a b c d value e;
fun set_PaymentOrder_direction (PaymentOrder a b c d e direction) value = PaymentOrder a b c d e value;
(*getters for PaymentOrder*)
fun get_PaymentOrder_amount (PaymentOrder amount _ _ _ _ _) = amount;
fun get_PaymentOrder_paymentTime (PaymentOrder _ paymentTime _ _ _ _) = paymentTime;
fun get_PaymentOrder_paymentId (PaymentOrder _ _ paymentId _ _ _) = paymentId;
fun get_PaymentOrder_taskId (PaymentOrder _ _ _ taskId _ _) = taskId;
fun get_PaymentOrder_paymentStatus (PaymentOrder _ _ _ _ paymentStatus _) = paymentStatus;
fun get_PaymentOrder_direction (PaymentOrder _ _ _ _ _ direction) = direction;

datatype Task = Task int Negotiation Person Person int int int int int int int int TaskStatus PaymentType;
(*setters for Task*)
fun set_task_id             (Task taskID  a b c d e f g h i k m n l) value =        Task value a b c d e f g h i k m n l;
fun set_task_negotiation    (Task a negotiation b c d e f g h i k m n l) value =    Task a value b c d e f g h i k m n l;
fun set_task_captain        (Task a b captain c d e f g h i k m n l) value =        Task a b value c d e f g h i k m n l;
fun set_task_worker         (Task a b c worker d e f g h i k m n l) value =         Task a b c value d e f g h i k m n l;
fun set_task_expectedGas    (Task a b c d expectedGas e f g h i k m n l) value =    Task a b c d value e f g h i k m n l;
fun set_task_requestedGas   (Task a b c d e requestedGas f g h i k m n l) value =   Task a b c d e value f g h i k m n l;
fun set_task_suppliedGas    (Task a b c d e f suppliedGas g h i k m n l) value =    Task a b c d e f value g h i k m n l;
fun set_task_totalGas       (Task a b c d e f g totalGas h i k m n l) value =       Task a b c d e f g value h i k m n l;
fun set_task_requestTime    (Task a b c d e f g h requestTime i k m n l) value =    Task a b c d e f g h value i k m n l;
fun set_task_suppliedTime   (Task a b c d e f g h i suppliedTime k m n l) value =   Task a b c d e f g h i value k m n l;
fun set_task_completionTime (Task a b c d e f g h i k completionTime m n l) value = Task a b c d e f g h i k value m n l;
fun set_task_paymentTime    (Task a b c d e f g h i k m paymentTime n l) value =    Task a b c d e f g h i k m value n l;
fun set_task_taskStatus     (Task a b c d e f g h i k m n taskStatus l) value =     Task a b c d e f g h i k m n value l;
fun set_task_paymentType    (Task a b c d e f g h i k m n l paymentType) value =    Task a b c d e f g h i k m n l value;
(*getters for Task*)
fun get_task_id (Task taskID _ _ _ _ _ _ _ _ _ _ _ _ _) = taskID;
fun get_task_negotiation (Task _ negotiation _ _ _ _ _ _ _ _ _ _ _ _) = negotiation;
fun get_task_captain (Task _ _ captain _ _ _ _ _ _ _ _ _ _ _) = captain;
fun get_task_worker (Task _ _ _ worker _ _ _ _ _ _ _ _ _ _) = worker;
fun get_task_expectedGas (Task _ _ _ _ expectedGas _ _ _ _ _ _ _ _ _) = expectedGas;
fun get_task_requestedGas (Task _ _ _ _ _ requestedGas _ _ _ _ _ _ _ _) = requestedGas;
fun get_task_suppliedGas (Task _ _ _ _ _ _ suppliedGas _ _ _ _ _ _ _) = suppliedGas;
fun get_task_totalGas (Task _ _ _ _ _ _ _ totalGas _ _ _ _ _ _ ) = totalGas;
fun get_task_requestTime (Task _ _ _ _ _ _ _ _ requestTime _ _ _ _ _ ) = requestTime;
fun get_task_suppliedTime (Task _ _ _ _ _ _ _ _ _ suppliedTime _ _ _ _ ) = suppliedTime;
fun get_task_completionTime (Task _ _ _ _ _ _ _ _ _ _ completionTime _ _ _ ) = completionTime;
fun get_task_paymentTime (Task _ _ _ _ _ _ _ _ _ _ _ paymentTime _ _ ) = paymentTime;
fun get_task_taskStatus (Task _ _ _ _ _ _ _ _ _ _ _ _ taskStatus _ ) = taskStatus;
fun get_task_paymentType (Task _ _ _ _ _ _ _ _ _ _ _ _ _ paymentType ) = paymentType;

datatype Campaign = Campaign Agreement (Task list) Negotiation (PriceChange list) Phase int (PaymentOrder list);
(*setters for Campaign*)
fun set_campaign_agreement (Campaign agreement a b c d e f) value = Campaign value a b c d e f;
fun set_campaign_tasks (Campaign a tasks b c d e f) value = Campaign a value b c d e f;
fun set_campaign_negotiation (Campaign a b negotiation c d e f) value = Campaign a b value c d e f;
fun set_campaign_priceChanges (Campaign a b c priceChanges d e f) value = Campaign a b c value d e f;
fun set_campaign_phase (Campaign a b c d phase e f) value = Campaign a b c d value e f;
fun set_campaign_bankAddress (Campaign a b c d e bankAddress f) value = Campaign a b c d e value f;
fun set_campaign_paymentOrders (Campaign a b c d e f paymentOrders) value = Campaign a b c d e f value;
(*getters for Campaign*)
fun get_campaign_agreement (Campaign agreement _ _ _ _ _ _) = agreement;
fun get_campaign_tasks (Campaign _ tasks _ _ _ _ _) = tasks;
fun get_campaign_negotiation (Campaign _ _ negotiation _ _ _ _) = negotiation;
fun get_campaign_priceChanges (Campaign _ _ _ priceChanges _ _ _) = priceChanges;
fun get_campaign_phase (Campaign _ _ _ _ phase _ _) = phase;
fun get_campaign_bankAddress (Campaign _ _ _ _ _ bankAddress _) = bankAddress;
fun get_campaign_paymentOrders (Campaign _ _ _ _ _ _ paymentOrders) = paymentOrders;
datatype SCType = TypeInt 
    | TypeString 
    | TypeBool 
    | TypeAgreement 
    | TypeTask
    | TypeNegotiation
    | TypePriceChange
    | TypePaymentOrder
    | TypeCampaign
    | TypePerson 
    | TypeList 
    | TypePaymentStatus
    | TypeAgreementDetails 
    | TypeTaskStatus 
    | TypePaymentType
    | TypePhase ;
    

datatype SCvalue = SCInt int
    | SCString string
    | SCBool bool
    | SCAgreement Agreement
    | SCTask Task
    | SCNegotiation Negotiation
    | SCPriceChange PriceChange
    | SCPaymentOrder PaymentOrder
    | SCCampaign Campaign
    | SCPerson Person
    | SCPaymentStatus PaymentStatus 
    | SCTaskStatus TaskStatus 
    | SCAgreementDetails AgreementDetails 
    | SCPaymentType PaymentType
    | SCPhase Phase ;

(* SCValue -> int option *)
fun scvalue_to_int x = 
    case x of 
    (SCInt x) => Some x
    | _ => None;
(* SCValue -> string option*)
fun scvalue_to_string x =
    case x of 
    (SCString x) => Some x
    | _ => None;
(* SCValue -> bool option*)
fun scvalue_to_bool x =
    case x of 
    (SCBool x) => Some x
    | _ => None;
(* SCValue -> AgreementDetails option*)
fun scvalue_to_agreementDetails x =
    case x of 
    (SCAgreementDetails x) => Some x
    | _ => None;
(*SCValue -> Negotiation option*)
fun scValue_to_negotiation x = 
    case x of 
    (SCNegotiation x) => Some x
    | _ => None;
(*SCValue -> Person option*)
fun scValue_to_person x = 
    case x of 
    (SCPerson x) => Some x
    | _ => None;
(*SCValue -> TaskStatus option*)
fun scValue_to_taskStatus x =
    case x of 
    (SCTaskStatus x) => Some x
    | _ => None;
(*SCValue -> Campaign option*)
fun scValue_to_campaign x =
    case x of 
    (SCCampaign x) => Some x
    | _ => None;
(*SCValue -> PaymentType option*)
fun scValue_to_paymentType x =
    case x of 
    (SCPaymentType x) => Some x
    | _ => None;
(*SCValue -> Task option*)
fun scValue_to_Task x =
    case x of 
    (SCTask x) => Some x
    | _ => None;
(*SCValue -> Agreement option*)
fun scValue_to_Agreement x =
    case x of 
    (SCAgreement x) => Some x
    | _ => None;
(*SCValue -> PriceChange option*)
fun scValue_to_PriceChange x =
    case x of 
    (SCPriceChange x) => Some x
    | _ => None;	(* byte_array -> (Word8.word list) *)
fun byte_array_to_w8list byteArray = 
  let 
    val count = ((Word8Array.length byteArray)-1)
    fun copy_from_array w8Array n = 
      if n = 0 then [Word8Array.sub w8Array n] else (Word8Array.sub w8Array n)::(copy_from_array w8Array (n-1))
  in
    List.rev (copy_from_array byteArray count)
  end;

(* string -> byte_array *)
fun string_to_w8array str = 
  let
    val newW8Array = Word8Array.array (String.size str) (Word8.fromInt 1)
  in
    (Word8Array.copyVec str 0 (String.size str) newW8Array 0 ; newW8Array)
  end;

(*(Word8.word list) -> Word8.word *)
fun w8CoW8 value = Word8.fromInt (List.length value) ;

(* SCType -> (Word8.word list) -> (Word8.word list) *)
fun encodeType someType value = 
    case someType of
          TypeString => [Word8.fromInt 1] @ value
        | TypeInt => [Word8.fromInt 2] @ value
        | TypeBool => [Word8.fromInt 3] @ value
        | TypeAgreement => [Word8.fromInt 4] @ value
        | TypeTask => [Word8.fromInt 5] @ value
        | TypeNegotiation => [Word8.fromInt 6] @ value
        | TypePriceChange => [Word8.fromInt 7] @ value
        | TypePaymentOrder => [Word8.fromInt 8] @ value
        | TypeCampaign => [Word8.fromInt 9] @ value
        | TypePerson => [Word8.fromInt 10] @ value 
        | TypeList => [Word8.fromInt 11] @ value  
        | TypePaymentStatus => [Word8.fromInt 12] @ value 
        | TypeAgreementDetails => [Word8.fromInt 13] @ value 
        | TypePhase => [Word8.fromInt 14] @ value 
        | TypeTaskStatus => [Word8.fromInt 15] @ value 
        | TypePaymentType => [Word8.fromInt 16] @ value ;

(* Negotiation -> (Word8.word list) *)
fun encodeNegotiation negotiation =
    let
      fun negotiation_to_int value =
        case value of
           NotSet => 1
         | WaitingCustomer => 2 
         | WaitingSupplier => 3  
         | NegotiationRejected => 4
         | NegotiationApproved => 5
    in
       encodeType TypeNegotiation ( [(w8CoW8 [(Word8.fromInt (negotiation_to_int negotiation))])] @ [(Word8.fromInt (negotiation_to_int negotiation))])
    end;

(* PaymentStatus -> (Word8.word list) *)
fun encodePaymentStatus paymentStatus =
    let
      fun paymentStatus_to_int value =
        case value of
           WaitingForPayment => 1
         | PaymentCompleted => 2 
         | PaymentRejected => 3  
    in
       encodeType TypePaymentStatus ( [(w8CoW8 [(Word8.fromInt (paymentStatus_to_int paymentStatus))])] @ [(Word8.fromInt (paymentStatus_to_int paymentStatus))])
    end;

(* bool -> (Word8.word list) *)
fun encodeBool value = 
    if value = True then 
        encodeType TypeBool ( [(w8CoW8 [(Word8.fromInt 1)])] @ [(Word8.fromInt 1)])
    else 
        encodeType TypeBool ( [(w8CoW8 [(Word8.fromInt 0)])] @ [(Word8.fromInt 0)]);

(* Phase -> (Word8.word list) *)
fun encodePhase phase =
    let
      fun phase_to_int value =
        case value of
           PhaseAgreement => 1
         | PhaseTasks => 2 
         | PhaseDeclined => 3  
    in
       encodeType TypePhase ( [(w8CoW8 [(Word8.fromInt (phase_to_int phase))])] @ [(Word8.fromInt (phase_to_int phase))])
    end;

(* Phase -> (Word8.word list) *)
fun encodeTaskStatus taskStatus =
    let
      fun taskStatus_to_int value =
        case value of
           TaskNotAccepted => 1
         | TaskAccepted => 2 
         | TaskReadyToPerform => 3  
         | GasRequested => 4
         | Performing => 5
         | Confirmed => 6
         | TaskCompleted => 7
    in
       encodeType TypeTaskStatus ( [(w8CoW8 [(Word8.fromInt (taskStatus_to_int taskStatus))])] @ [(Word8.fromInt (taskStatus_to_int taskStatus))])
    end;

(* PaymentType -> (Word8.word list) *)
fun encodePaymentType paymentType =
    let
      fun paymentType_to_int value =
        case value of
           Pre => 1
         | Post => 2 
         | Delayed => 3  
    in
       encodeType TypePaymentType ( [(w8CoW8 [(Word8.fromInt (paymentType_to_int paymentType))])] @ [(Word8.fromInt (paymentType_to_int paymentType))])
    end;

(* (Word8.word list) -> int -> (Word8.word list) *)
fun encode_position w8list position = 
    case w8list of
       [] => []
     | (x::tail) => x::(Word8.fromInt position)::tail;

(* string -> (Word8.word list) *)
fun encodeString str = encodeType TypeString ( [(w8CoW8 (byte_array_to_w8list (string_to_w8array str)))] @ (byte_array_to_w8list (string_to_w8array str)));

(* int -> (Word8.word list) *)
fun encodeInt number = 
    let
      fun encodeIntlocal integer = 
        if integer = 0 then [] 
        else [(Word8.fromInt (Int.mod integer 256))] @ (encodeIntlocal (Int.div integer 256))
      val w8l = encodeIntlocal number
    in
     encodeType TypeInt ([(w8CoW8 w8l)] @ w8l)
    end;

(* Person -> (Word8.word list) *)
fun encodePerson person = 
    case person of
     (Person addr name) => 
         let
          val w8fields = ((encode_position (encodeInt addr) 1) @ 
                          (encode_position (encodeString name) 2))
                         
        in
          encodeType TypePerson ( [w8CoW8 w8fields] @ w8fields )
        end;

(* AgreementDetails -> (Word8.word list) *)
fun encodeAgreementDetails agreementDetails = 
    case agreementDetails of
     (AgreementDetails details bankAddress) => 
        let
          val w8fields = (encode_position (encodeString details) 1) @ 
                         (encode_position (encodeInt bankAddress) 2) 
        in
          encodeType TypeAgreementDetails ( [w8CoW8 w8fields] @ w8fields )
        end;

(* PriceChange -> (Word8.word list) *)
fun encodePriceChange priceChange = 
    case priceChange of
     (PriceChange price negotiation startTime) => 
        let
          val w8fields = (encode_position (encodeInt price) 1) @ 
                         (encode_position (encodeNegotiation negotiation) 2) @ 
                         (encode_position (encodeInt startTime) 3) 
        in
          encodeType TypePriceChange ( [w8CoW8 w8fields] @ w8fields )
        end;

(* PaymentOrder -> (Word8.word list) *)
fun encodePaymentOrder paymentOrder = 
    case paymentOrder of
     (PaymentOrder amount paymentTime paymentId taskId paymentStatus direction) => 
        let
          val w8fields = (encode_position (encodeInt amount) 1) @ 
                         (encode_position (encodeInt paymentTime) 2) @ 
                         (encode_position (encodeInt paymentId) 3) @ 
                         (encode_position (encodeInt taskId) 4) @ 
                         (encode_position (encodePaymentStatus paymentStatus) 5) @
                         (encode_position (encodeBool direction) 6)
        in
          encodeType TypePaymentOrder ( [w8CoW8 w8fields] @ w8fields )
        end;

(* Agreement -> (Word8.word list) *)
fun encodeAgreement agreement = 
    case agreement of
     (Agreement negotiation customer supplier details) => 
        let
          val w8fields = (encode_position (encodeNegotiation negotiation) 1) @ 
                         (encode_position (encodePerson customer) 2) @ 
                         (encode_position (encodePerson supplier) 3) @ 
                         (encode_position (encodeAgreementDetails details) 4) 
        in
          encodeType TypeAgreement ( [w8CoW8 w8fields] @ w8fields )
        end;

(* Task -> (Word8.word list) *)
fun encodeTask task = 
    case task of
     (Task taskId negotiation captain worker expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType) => 
        let
          val w8fields = (encode_position (encodeInt taskId) 1) @
                         (encode_position (encodeNegotiation negotiation) 2) @ 
                         (encode_position (encodePerson captain) 3) @ 
                         (encode_position (encodePerson worker) 4) @ 
                         (encode_position (encodeInt expectedGas) 5) @
                         (encode_position (encodeInt requestedGas) 6) @
                         (encode_position (encodeInt suppliedGas) 7) @
                         (encode_position (encodeInt totalGas) 8) @
                         (encode_position (encodeInt requestTime) 9) @
                         (encode_position (encodeInt suppliedTime) 10) @
                         (encode_position (encodeInt completionTime) 11) @
                         (encode_position (encodeInt paymentTime) 12) @
                         (encode_position (encodeTaskStatus taskStatus) 13) @
                         (encode_position (encodePaymentType paymentType) 14)
        in
          encodeType TypeTask ( [w8CoW8 w8fields] @ w8fields )
        end;

(* Campaign -> (Word8.word list) *)
fun encodeCampaign campaign = 
    case campaign of
     (Campaign agreement tasks negotiation priceChanges phase bankAddress paymentOrders) => 
        let
          fun encode_list_task_felds some_list func position = 
            case some_list of
               [] => []
             | (x::tail) => (encode_position (encodeType TypeList (List.tl (func x)) ) position) @ (encode_list_task_felds tail func position)
          fun encode_list_priceChange_felds some_list func position = 
            case some_list of
               [] => []
             | (x::tail) => (encode_position (encodeType TypeList (List.tl (func x))) position) @ (encode_list_priceChange_felds tail func position)
        fun encode_list_paymentOrder_felds some_list func position = 
            case some_list of
               [] => []
             | (x::tail) => (encode_position (encodeType TypeList (List.tl (func x))) position) @ (encode_list_paymentOrder_felds tail func position)
             
          val w8fields = (encode_position (encodeAgreement agreement) 1) @
                         (encode_list_task_felds tasks encodeTask 2) @ 
                         (encode_position (encodeNegotiation negotiation) 3) @ 
                         (encode_list_priceChange_felds priceChanges encodePriceChange 4) @ 
                         (encode_position (encodePhase phase) 5) @
                         (encode_position (encodeInt bankAddress) 6) @
                         (encode_list_paymentOrder_felds paymentOrders encodePaymentOrder 7)
        in
          encodeType TypeCampaign ( [w8CoW8 w8fields] @ w8fields )
        end;

(* SCValue -> (Word8.word list) *)
fun encodeValue value = 
    case value of 
       SCString str => encodeString str
     | SCInt integer => encodeInt integer
     | SCBool someBool => encodeBool someBool
     | SCNegotiation negotiation => encodeNegotiation negotiation
     | SCPaymentStatus paymentStatus => encodePaymentStatus paymentStatus
     | SCAgreementDetails agreementDetails => encodeAgreementDetails agreementDetails
     | SCPerson person => encodePerson person
     | SCPaymentOrder paymentOrder => encodePaymentOrder paymentOrder
     | SCPriceChange priceChange => encodePriceChange priceChange 
     | SCPhase phase => encodePhase phase 
     | SCTaskStatus taskStatus => encodeTaskStatus taskStatus 
     | SCPaymentType paymentType => encodePaymentType paymentType 
     | SCAgreement agreement => encodeAgreement agreement 
     | SCTask task => encodeTask task 
     | SCCampaign campaign => encodeCampaign campaign;fun w8listToArray w8list = 
  let 
    val newW8Array = Word8Array.array ((List.length w8list)) (Word8.fromInt 1)
    fun fillArray (x::tail) w8Array n = 
      if tail = [] then (Word8Array.update w8Array n x; w8Array) else 
        (Word8Array.update w8Array n x; fillArray tail w8Array (n+1))
  in
    fillArray w8list newW8Array 0
  end;

(*(word8)-> SCtype *)
fun decodeType value = 
    case (Word8.toInt value) of
          1 => TypeString 
        | 2 => TypeInt
        | 3 => TypeBool 
        | 4 => TypeAgreement 
        | 5 => TypeTask
        | 6 => TypeNegotiation
        | 7 => TypePriceChange
        | 8 => TypePaymentOrder
        | 9 => TypeCampaign
        | 10 => TypePerson 
        | 11 => TypeList 
        | 12 => TypePaymentStatus
        | 13 => TypeAgreementDetails 
        | 14 => TypePhase 
        | 15 => TypeTaskStatus 
        | 16 => TypePaymentType ; 

(* (Word8.word list) -> int количество байтов содержащих это значение *)
fun countOfW8 (x::tail) = Word8.toInt x ;

(* (Word8.word list), n -> (list w8) первые n символов *)
fun cutTail (x::tail) n = if n = (List.length (x::tail)) then x::tail else (if n = 0 then [] else x::(cutTail tail (n-1))) ;

(* (Word8.word list), n -> (list w8) остаток листа после n символов *)
fun cutHead (x::tail) n = if n = (List.length (x::tail)) then [] else (if n = 0 then x::tail else (cutHead tail (n-1))) ;

(* (Word8.word list) -> String *)
fun decodeString w8list = 
    case w8list of
       [] => ""
     | w8list => (Word8Array.substring (w8listToArray w8list) 0 (Word8Array.length (w8listToArray w8list))) ;

(* (Word8.word list) -> Int -> Int *)
fun decodeInt value n = 
    case value of 
       [] => n
     | (x::xs) => n+((Word8.toInt x)+(256*(decodeInt xs n)));
     
(* Word8.word -> bool *)
fun decodeBool value = 
    case value of 
       [] => False
     | (x::tail) => case (Word8.toInt x) of 
                       0 => False
                     | 1=> True ;

(* (Word8.word list) Negotiation -> Negotiation *)
fun decodeNegotiation w8List negotiation = 
    case w8List of
       [] => negotiation
     | (x::tail) => 
                let
                  fun negotiation_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => NotSet
                     | 2 => WaitingCustomer
                     | 3 => WaitingSupplier
                     | 4 => NegotiationRejected
                     | 5 => NegotiationApproved
                in
                 negotiation_string_fields (Word8.toInt x) tail
                end;

(* (Word8.word list) -> PaymentStatus -> PaymentStatus *)
fun decodePaymentStatus w8List paymentStatus = 
    case w8List of
       [] => paymentStatus
     | (x::tail) => 
                let
                  fun paymentStatus_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => WaitingForPayment
                     | 2 => PaymentCompleted
                     | 3 => PaymentRejected
                in
                 paymentStatus_string_fields (Word8.toInt x) tail
                end; 

(* (Word8.word list) -> Phase -> Phase *)
fun decodePhase w8List phase = 
    case w8List of
       [] => phase
     | (x::tail) => 
                let
                  fun phase_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => PhaseAgreement
                     | 2 => PhaseTasks
                     | 3 => PhaseDeclined
                in
                 phase_string_fields (Word8.toInt x) tail
                end; 

(* (Word8.word list) -> TaskStatus -> TaskStatus *)
fun decodeTaskStatus w8List taskStatus = 
    case w8List of
       [] => taskStatus
     | (x::tail) => 
                let
                  fun taskStatus_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => TaskNotAccepted
                     | 2 => TaskAccepted 
                     | 3 => TaskReadyToPerform
                     | 4 => GasRequested
                     | 5 => Performing
                     | 6 => Confirmed
                     | 7 => TaskCompleted
                in
                 taskStatus_string_fields (Word8.toInt x) tail
                end;

(* (Word8.word list) -> Phase -> Phase *)
fun decodePaymentType w8List paymentType = 
    case w8List of
       [] => paymentType
     | (x::tail) => 
                let
                  fun paymentType_string_fields number_of_field without_number =
                    case number_of_field of
                       1 => Pre
                     | 2 => Post
                     | 3 => Delayed 
                in
                 paymentType_string_fields (Word8.toInt x) tail
                end; 

(* (Word8.word list) -> Person -> Person *)
fun decodePerson w8List person =
    case w8List of
       [] => person
     | (x::tail) => 
          case (decodeType x) of 
             TypeInt =>
               let
                fun person_integer_fields number_of_field without_number =
                  case number_of_field of
                    1 => decodePerson (cutHead (List.tl without_number) (countOfW8 without_number)) (set_person_addr person (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
               in
                person_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypeString => 
               let
                fun person_string_fields number_of_field without_number =
                  case number_of_field of
                     2 => decodePerson (cutHead (List.tl without_number) (countOfW8 without_number)) (set_person_name person (decodeString (cutTail (List.tl without_number) (countOfW8 without_number))))
               in
                 person_string_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end;

(* (Word8.word list) -> AgreementDetails -> AgreementDetails *)
fun decodeAgreementDetails w8List agreementDetails =
    case w8List of
       [] => agreementDetails
     | (x::tail) => 
          case (decodeType x) of 
             TypeInt =>
               let
                fun agreementDetails_integer_fields number_of_field without_number =
                  case number_of_field of
                     2 => decodeAgreementDetails (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreementDetails_bankAddress agreementDetails (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
               in
                agreementDetails_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypeString => 
               let
                fun agreementDetails_string_fields number_of_field without_number =
                  case number_of_field of
                     1 => decodeAgreementDetails (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreementDetails_details agreementDetails (decodeString (cutTail (List.tl without_number) (countOfW8 without_number))))
               in
                 agreementDetails_string_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end;


(* (Word8.word list) PaymentOrder -> PaymentOrder *)
fun decodePaymentOrder  w8List paymentOrder = 
    case w8List of
       [] => paymentOrder
     | (x::tail) => case (decodeType x) of 
             TypeInt => 
               let
                fun paymentOrder_integer_fields number_of_field without_number =
                  case number_of_field of
                     1 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_amount paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
                   | 2 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_paymentTime paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
                   | 3 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_paymentId paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                   | 4 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_taskId paymentOrder (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
               in
                paymentOrder_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypePaymentStatus => 
               let
                fun paymentOrder_PaymentStatus_fields number_of_field without_number =
                  case number_of_field of
                     5 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_paymentStatus paymentOrder (decodePaymentStatus (cutTail (List.tl without_number) (countOfW8 without_number)) (WaitingForPayment)))
               in
                paymentOrder_PaymentStatus_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
            | TypeBool => 
               let
                fun paymentOrder_Bool_fields number_of_field without_number =
                  case number_of_field of
                     6 => decodePaymentOrder (cutHead (List.tl without_number) (countOfW8 without_number)) (set_PaymentOrder_direction paymentOrder (decodeBool (cutTail (List.tl without_number) (countOfW8 without_number)) ))
               in
                paymentOrder_Bool_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end;

(* (Word8.word list) Agreement -> Agreement *)
fun decodeAgreement w8List agreement =
    case w8List of
       [] => agreement
     | (x::tail) => 
        case (decodeType x) of
           TypeNegotiation => 
            let
             fun agreement_Negotiation_fields number_of_field without_number =
                case number_of_field of
                   1 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_negotiation agreement (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
            in
             agreement_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end
         | TypePerson => 
            let
             fun agreement_Person_fields number_of_field without_number =
                case number_of_field of
                   2 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_customer agreement (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
                 | 3 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_supplier agreement (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
            in
             agreement_Person_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end 
         |  TypeAgreementDetails => 
            let
             fun agreement_agreementDetails_fields number_of_field without_number =
                case number_of_field of
                   4 => decodeAgreement (cutHead (List.tl without_number) (countOfW8 without_number)) (set_agreement_details agreement (decodeAgreementDetails (cutTail (List.tl without_number) (countOfW8 without_number)) (AgreementDetails "" 0) )) 
            in
             agreement_agreementDetails_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end ;

(* (Word8.word list) Task -> Task *)
fun decodeTask w8List task = 
    case w8List of
       [] => task
     | (x::tail) => 
        case (decodeType x) of  
           TypeNegotiation => 
            let
             fun task_Negotiation_fields number_of_field without_number =
                case number_of_field of
                   2 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_negotiation task (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
            in
             task_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end
         | TypePerson => 
            let
             fun task_Person_fields number_of_field without_number =
                case number_of_field of
                   3 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_captain task (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
                 | 4 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_worker task (decodePerson (cutTail (List.tl without_number) (countOfW8 without_number)) (Person 0 "")))
            in
             task_Person_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end 
         | TypeInt =>
             let
              fun task_integer_fields number_of_field without_number =
                case number_of_field of
                   1 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_id task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 5 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_expectedGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 6 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_requestedGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 7 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_suppliedGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 8 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_totalGas task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 9 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_requestTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 10 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_suppliedTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 11 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_completionTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                 | 12 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_paymentTime task (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
            in
              task_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end
         | TypeTaskStatus =>
             let
               fun task_TaskStatus_fields number_of_field without_number = 
                case number_of_field of 
                   13 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_taskStatus task (decodeTaskStatus (cutTail (List.tl without_number) (countOfW8 without_number)) (TaskNotAccepted)))
             in
               task_TaskStatus_fields (Word8.toInt (List.hd tail)) (List.tl tail)
             end
         | TypePaymentType =>
             let
               fun task_PaymentType_fields number_of_field without_number = 
                case number_of_field of 
                   14 => decodeTask (cutHead (List.tl without_number) (countOfW8 without_number)) (set_task_paymentType task (decodePaymentType (cutTail (List.tl without_number) (countOfW8 without_number)) (Pre)))
             in
               task_PaymentType_fields (Word8.toInt (List.hd tail)) (List.tl tail)
             end;

(* (Word8.word list) PriceChange -> PriceChange *)
fun decodePriceChange w8List priceChange = 
    case w8List of
       [] => priceChange
     | (x::tail) => 
          case (decodeType x) of 
             TypeInt =>
               let
                fun priceChange_integer_fields number_of_field without_number =
                  case number_of_field of
                     1 => decodePriceChange (cutHead (List.tl without_number) (countOfW8 without_number)) (set_priceChange_price priceChange (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0)) 
                   | 3 => decodePriceChange (cutHead (List.tl without_number) (countOfW8 without_number)) (set_priceChange_startTime priceChange (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number))0))
               in
                priceChange_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
               end
           | TypeNegotiation => 
            let
             fun agreement_Negotiation_fields number_of_field without_number =
                case number_of_field of
                   2 => decodePriceChange (cutHead (List.tl without_number) (countOfW8 without_number)) (set_priceChange_negotiation priceChange (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
            in
            agreement_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
            end;
 
(* (Word8.word list) Campaign -> Campaign *)
fun decodeCampaign w8List campaign = 
    case w8List of
       [] => campaign
     | (x::tail) => 
          case (decodeType x) of 
             TypeAgreement => 
                let
                  fun campaign_Agreement_fields number_of_field without_number =
                   case number_of_field of
                      1 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_agreement campaign (decodeAgreement (cutTail (List.tl without_number) (countOfW8 without_number)) (Agreement NotSet (Person 0 "") (Person 0 "") (AgreementDetails "" 0))))
                in
                  campaign_Agreement_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypeNegotiation => 
                let
                  fun campaign_Negotiation_fields number_of_field without_number =
                   case number_of_field of
                      3 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_negotiation campaign (decodeNegotiation (cutTail (List.tl without_number) (countOfW8 without_number)) (NotSet)))
                in
                  campaign_Negotiation_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypeList =>
                let
                  fun campaign_list_fields number_of_field without_number = 
                   case number_of_field of 
                      2 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_tasks campaign ((get_campaign_tasks campaign) @ [(decodeTask (cutTail (List.tl without_number) (countOfW8 without_number)) (Task 0 NotSet (Person 0 "") (Person 0 "") 0 0 0 0 0 0 0 0 TaskNotAccepted Pre))]))
                    | 4 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [(decodePriceChange (cutTail (List.tl without_number) (countOfW8 without_number)) (PriceChange 0 NotSet 0 ))]))
                    | 7 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_paymentOrders campaign ((get_campaign_paymentOrders campaign) @ [(decodePaymentOrder (cutTail (List.tl without_number) (countOfW8 without_number)) (PaymentOrder 0 0 0 0 WaitingForPayment True))]))
                in
                  campaign_list_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypePhase => 
                let
                  fun campaign_Phase_fields number_of_field without_number =
                   case number_of_field of
                      5 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_phase campaign (decodePhase (cutTail (List.tl without_number) (countOfW8 without_number)) (PhaseAgreement)))
                in
                  campaign_Phase_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end
            | TypeInt => 
                let
                  fun campaign_integer_fields number_of_field without_number =
                   case number_of_field of
                      6 => decodeCampaign (cutHead (List.tl without_number) (countOfW8 without_number)) (set_campaign_bankAddress campaign (decodeInt (cutTail (List.tl without_number) (countOfW8 without_number)) 0))
                in
                  campaign_integer_fields (Word8.toInt (List.hd tail)) (List.tl tail)
                end;

(* (list word8)->SCvalue *)
fun decodeValue value = 
    case value of (x::tail) => case (decodeType x) of TypeString =>  SCString (decodeString tail)
            | TypeInt => SCInt (decodeInt tail 0) 
            | TypeBool => SCBool (decodeBool (List.tl tail))
            | TypeAgreement => SCAgreement
                let
                   val newAgreement = Agreement NotSet (Person 0 "") (Person 0 "") (AgreementDetails "" 0)
                in
                    decodeAgreement (List.tl tail) newAgreement
                end 
            | TypeTask => SCTask 
                let
                    val newTask = Task 0 NotSet (Person 0 "") (Person 0 "") 0 0 0 0 0 0 0 0 TaskNotAccepted Pre
                in
                   decodeTask (List.tl tail) newTask
                end 
            | TypeNegotiation => SCNegotiation
                let
                   val newNegotiation = NotSet
                in
                   decodeNegotiation (List.tl tail) newNegotiation
                end 
            | TypePhase => SCPhase
                let
                   val newPhase = PhaseAgreement
                in
                   decodePhase (List.tl tail) newPhase
                end 
            | TypePaymentType => SCPaymentType
                let
                   val newPaymentType = Pre
                in
                   decodePaymentType (List.tl tail) newPaymentType
                end 
            | TypeTaskStatus => SCTaskStatus 
                let
                   val newTaskStatus = TaskNotAccepted
                in
                   decodeTaskStatus (List.tl tail) newTaskStatus
                end 
            | TypePaymentStatus => SCPaymentStatus     
                let
                   val newPaymentStatus = WaitingForPayment
                in
                   decodePaymentStatus (List.tl tail) newPaymentStatus
                end 
            | TypePriceChange => SCPriceChange 
                let
                   val newPriceChange = PriceChange 0 NotSet 0 
                in
                    decodePriceChange (List.tl tail) newPriceChange
                end 
            | TypePaymentOrder => SCPaymentOrder 
                let
                   val newPaymentOrder = PaymentOrder 0 0 0 0 WaitingForPayment True
                in
                    decodePaymentOrder (List.tl tail) newPaymentOrder
                end 
            | TypeCampaign => SCCampaign 
                let
                   val newCampaign = Campaign (Agreement NotSet (Person 0 "") (Person 0 "") (AgreementDetails "" 0)) [] NotSet [] PhaseAgreement 0 []
                in
                    decodeCampaign (List.tl tail) newCampaign
                end 
            | TypeAgreementDetails => SCAgreementDetails
                let
                   val newAgreementDetails = AgreementDetails "" 0
                in
                    decodeAgreementDetails (List.tl tail) newAgreementDetails
                end 
            | TypePerson => SCPerson 
                let
                   val newPerson = Person 0 ""
                in
                    decodePerson (List.tl tail) newPerson
                end ;fun extract somelist foo = 
    case somelist of
    [] => "[]"
    | (x::tail) => if tail = [] then foo x else foo x ^ extract tail foo;

fun negotiation_toString x = 
    if x = NotSet then 
        "NotSet" 
    else if x = WaitingCustomer then 
        "WaitingCustomer"
    else if x = WaitingSupplier then 
        "WaitingSupplier" 
    else if x = NegotiationRejected then 
        "NegotiationRejected" 
    else "NegotiationApproved" 

fun negotiation_toPrettyString x =
    "\t\t\tNgtn: " ^ negotiation_toString x ^ "\n";

(* PaymentStatus -> string *)
fun paymentStatus_toString x = 
    if x = WaitingForPayment then 
        "WaitingForPayment" 
    else if x = PaymentCompleted then 
        "PaymentCompleted"
    else "PaymentRejected" 

(* string -> string *)
fun paymentStatus_toPrettyString x =
    "\t\t\tPaymentStatus: " ^ paymentStatus_toString x ^ "\n";
    
(* Phase -> string *)
fun phase_toString x = 
    case x of
       PhaseAgreement => "PhaseAgreement"
     | PhaseTasks => "PhaseTasks"
     | PhaseDeclined => "PhaseDeclined";

(* string -> string *)
fun phase_toPrettyString x =
    "\t\t\tPhase: " ^ phase_toString x ^ "\n";

(* Phase -> string *)
fun taskStatus_toString x = 
    case x of
       TaskNotAccepted => "TaskNotAccepted"
     | TaskAccepted => "TaskAccepted"
     | TaskReadyToPerform => "TaskReadyToPerform"
     | GasRequested => "GasRequested"
     | Performing => "Performing"
     | Confirmed => "Confirmed"
     | TaskCompleted => "TaskCompleted";

(* string -> string *)
fun taskStatus_toPrettyString x =
    "\t\t\tTaskStatus: " ^ taskStatus_toString x ^ "\n";

(* PaymentType -> string *)
fun paymentType_toString x = 
    case x of
       Pre => "Pre"
     | Post => "Post"
     | Delayed => "Delayed";

(* string -> string *)
fun paymentType_toPrettyString x =
    "\t\t\tPaymentType: " ^ paymentType_toString x ^ "\n";

(* PriceChange string functions *)

(* PriceChange -> string *)
fun priceChange_toString x =
    case x of PriceChange price negotiation startTime =>
    Int.toString price ^
    negotiation_toString negotiation ^
    Int.toString startTime;

(* string -> string *)
fun priceChange_toPrettyString x =
    case x of PriceChange price negotiation startTime =>
    "\t\t\tPrce: " ^ Int.toString price ^ "\n" ^
    "\t\t\tNgtn: " ^ negotiation_toPrettyString negotiation ^ "\n" ^
    "\t\t\tStTm: " ^ Int.toString startTime ^ "\n";

(* Person string functions *) 

(* Person -> string *)
fun person_toString x =
    case x of Person addr name =>
    Int.toString addr ^
    name;  

(* string -> string *)
fun person_toPrettyString x =
    case x of Person addr name =>
    "\t\t\taddr: " ^ Int.toString addr ^ "\n" ^
    "\t\t\tName: " ^ name ^ "\n";

(* AgreementDetails string functions *) 

(* AgreementDetails -> string *)
fun agreementDetails_toString x =
    case x of AgreementDetails details bankAddress =>
    details ^
    Int.toString bankAddress;    

(* string -> string *)
fun agreementDetails_toPrettyString x =
    case x of AgreementDetails details bankAddress =>
    "\t\t\tName: " ^ details ^ "\n" ^
    "\t\t\taddr: " ^ Int.toString bankAddress ^ "\n";

(* Agreement string functions *) 
fun agreement_toString x =
    case x of Agreement negotiation customer supplier details =>
    negotiation_toString negotiation ^
    person_toString customer ^
    person_toString supplier ^
    agreementDetails_toString details;    

fun agreement_toPrettyString x =
    case x of Agreement negotiation customer supplier details =>
    "\t\t\tNgtn: " ^ negotiation_toPrettyString negotiation ^ "\n" ^
    "\t\t\tCstm: " ^ person_toPrettyString customer ^ "\n" ^
    "\t\t\tSplr: " ^ person_toPrettyString supplier ^ "\n" ^
    "\t\t\tDtls: " ^ agreementDetails_toPrettyString details ^ "\n";

(* PaymentOrder string functions *)
fun paymentOrder_toString x =
    case x of PaymentOrder amount paymentTime paymentId taskId paymentStatus direction =>
    Int.toString amount ^
    Int.toString paymentTime ^
    Int.toString paymentId ^
    paymentStatus_toPrettyString paymentStatus;    

fun paymentOrder_toPrettyString x =
    case x of PaymentOrder amount paymentTime paymentId taskId paymentStatus direction =>
    "\t\t\tAmount: " ^ Int.toString amount ^ "\n" ^
    "\t\t\tPmTime: " ^ Int.toString paymentTime ^ "\n" ^
    "\t\t\tPmIndx: " ^ Int.toString paymentId ^ "\n" ^
     paymentStatus_toPrettyString paymentStatus ;

(* Task string functions *)
fun task_toString x =
    case x of Task taskId negotiation captain worker expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType => 
    Int.toString taskId ^ 
    negotiation_toString negotiation ^
    person_toString captain ^ 
    person_toString worker ^ 
    Int.toString expectedGas ^ 
    Int.toString requestedGas ^ 
    Int.toString suppliedGas ^ 
    Int.toString totalGas ^
    Int.toString requestTime ^ 
    Int.toString suppliedTime ^ 
    Int.toString completionTime ^ 
    Int.toString paymentTime ^ 
    taskStatus_toString taskStatus ^
    paymentType_toString paymentType ;

fun task_toPrettyString x =
    case x of Task taskId negotiation captain worker expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType => 
    "\t TskId:" ^ Int.toString taskId ^ "\n" ^ 
    "\t Negotiation:" ^ negotiation_toString negotiation ^ "\n" ^ 
    "\t Captn:\n" ^ person_toPrettyString captain ^ "\n" ^ 
    "\t Workr:\n" ^ person_toPrettyString worker ^ "\n" ^ 
    "\t ExpGs:" ^ Int.toString expectedGas ^ "\n" ^ 
    "\t ReqGs:" ^ Int.toString requestedGas ^ "\n" ^ 
    "\t SupGs:" ^ Int.toString suppliedGas ^ "\n" ^ 
    "\t totalGas:" ^ Int.toString totalGas ^ "\n" ^ 
    "\t ReqTm:" ^ Int.toString requestTime ^ "\n" ^ 
    "\t SupTm:" ^ Int.toString suppliedTime ^ "\n" ^ 
    "\t CmpTm:" ^ Int.toString completionTime ^ "\n" ^ 
    "\t paymentTime:" ^ Int.toString paymentTime ^ "\n" ^
    taskStatus_toPrettyString taskStatus ^
    "\t paymentType:" ^ paymentType_toString paymentType ^ "\n" ;

(* Campaign string functions *)
fun campaign_toString x = (* To DO: Pretty print for agreement |*)
    case x of Campaign agreement tasks negotiation priceChanges phase bankAddress paymentOrders => 
    agreement_toString agreement  ^ 
    extract tasks task_toString ^ 
    negotiation_toPrettyString negotiation ^ 
    extract priceChanges priceChange_toString ^
    phase_toString phase ^  
    Int.toString bankAddress ^
    extract paymentOrders paymentOrder_toString;

fun campaign_toPrettyString x = (* to do Pretty print for agreement |*)
    case x of Campaign agreement tasks negotiation priceChanges phase bankAddress paymentOrders => 
    "Agrmt: " ^ agreement_toPrettyString agreement  ^ "\n" ^
    "Tasks:\n" ^ extract tasks task_toPrettyString^
    "Negot: " ^ negotiation_toPrettyString negotiation ^ "\n" ^
    "PrChn: " ^ extract priceChanges priceChange_toPrettyString ^ "\n" ^
    "Phase: " ^ phase_toPrettyString phase ^ "\n" ^
    "bankAddress: " ^ Int.toString bankAddress ^ "\n" ^ 
    "paymentOrders: " ^ extract paymentOrders paymentOrder_toPrettyString ^ "\n" ;structure ContractPrivate = 
struct
    fun p_get_campaign_task_by_task_id campaign taskId = 
    let
        fun get_task (x::tail) taskId =
            if (get_task_id x) = taskId then
                Some x
            else if tail = [] then 
                None                
            else
                get_task (tail) taskId;
    in 
        get_task (get_campaign_tasks campaign) taskId
    end;

fun p_get_last_price_change campaign = 
    let 
        fun get_price_change (x::tail) =
            if tail = [] then 
                x
            else
                get_price_change tail;
    in
        if (get_campaign_priceChanges campaign) = [] then 
            None
        else
            Some (get_price_change (get_campaign_priceChanges campaign))
    end;

(*functions for Agreement*)
fun update_agreement_details agreement details = 
    set_agreement_details agreement details;

fun approve_agreement agreement = 
    set_agreement_negotiation agreement NegotiationApproved;

fun reject_agreement agreement = 
    set_agreement_negotiation agreement NegotiationRejected;

(*functions for Campaign*)
fun p_update_Campaign_tasks campaign task taskID = 
    let
        fun change_task (x::tail) task taskID = 
            if get_task_id x = taskID then 
                task::tail 
            else 
                x::(change_task tail task taskID)
    in
        set_campaign_tasks (campaign) (change_task (get_campaign_tasks campaign) task taskID) 
    end;

fun p_add_task_in_Campaign campaign task = 
    set_campaign_tasks campaign ((get_campaign_tasks campaign) @ [task]);

fun waitCustomer_in_Campaign campaign = 
    set_campaign_negotiation campaign WaitingCustomer;

fun waitSupplier_in_Campaign campaign = 
    set_campaign_negotiation campaign WaitingSupplier;

fun reject_tasks_in_Campaign campaign = 
    set_campaign_negotiation campaign NegotiationRejected;

fun approve_tasks_in_Campaign campaign = 
    set_campaign_negotiation campaign NegotiationApproved;

fun p_remove_task_from_Campaign campaign taskID =
    let 
        fun remove_task_by_id (x::tail) taskID = 
            if get_task_id x = taskID then 
                tail 
            else 
                x::(remove_task_by_id tail taskID)
    in
        set_campaign_tasks campaign (remove_task_by_id (get_campaign_tasks campaign) taskID)
    end;

fun p_get_paymentOrder_by_id campaign paymentId = 
    let 
        fun get_paymentOrder (x::tail) paymentId = 
            if (get_PaymentOrder_paymentId x) = paymentId then
                Some x 
            else if tail = [] then
                None
            else
                get_paymentOrder (tail) paymentId;
    in
        get_paymentOrder (get_campaign_paymentOrders campaign) paymentId
    end;

fun p_update_paymentOrder_by_id campaign paymentOrder paymentId = 
    let
        fun change_paymentOrder (x::tail) paymentOrder paymentId = 
            if (get_PaymentOrder_paymentId x) = paymentId then 
                paymentOrder::tail 
            else 
                x::(change_paymentOrder tail paymentOrder paymentId);
    in
        set_campaign_paymentOrders campaign (change_paymentOrder (get_campaign_paymentOrders campaign) paymentOrder paymentId)
    end;

fun change_price_campaign campaign priceChange = 
    set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [priceChange])

fun approve_price_campaign campaign = 
    let 
        fun approve_last_priceChange (x::tail) = 
            if tail = [] then 
                [set_priceChange_negotiation x NegotiationApproved]
            else 
                x::(approve_last_priceChange tail)
    in
        set_campaign_priceChanges campaign ( approve_last_priceChange (get_campaign_priceChanges campaign))
    end;

fun p_perform campaign taskId neededGas = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_requestTime (set_task_requestedGas x neededGas) 0)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

fun p_completed campaign taskId suppliedGas = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_suppliedTime (set_task_suppliedGas x suppliedGas) 0)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

fun p_confirm campaign taskId = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_completionTime x 0)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

fun calculateLastPrice campaign = 
    let 
        fun get_last_approved arr = 
            if (get_priceChange_negotiation (List.last arr)) = NegotiationApproved then 
                Some (get_priceChange_price (List.last arr))
            else if List.length arr = 1 then
                None
            else
                get_last_approved (List.take arr ((List.length arr) - 1))
    in 
        get_last_approved (get_campaign_priceChanges campaign)
    end;


(* (Task list) -> int -> Task *)
fun p_get_task_by_id taskList taskId = 
    case taskList of
       [] => NONE ("Task not found")
     | (x::tail) => 
        if get_task_id x = taskId then 
           SOME x 
        else 
            p_get_task_by_id tail taskId;

fun get_element_with_number (x::tail) number = if number = 0 then x else get_element_with_number tail (number-1);
    
(* a' list -> a' *)
fun get_last_element (x::tail) = if tail = [] then x else get_last_element tail;

(* a' list -> a' list *)
fun cut_last_element (x::tail) = if tail = [] then [] else x::(cut_last_element tail);

(* Campaign -> int -> Campaign *)
fun p_approve_task campaign taskId = 
        let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_negotiation x NegotiationApproved)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

(* Campaign -> int -> Campaign *)
fun p_reject_task campaign taskId = 
        let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_negotiation x NegotiationRejected)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;

(* Campaign -> int -> Campaign *)
fun p_accept_task campaign taskId = 
    let
        fun change_task (x::tail) taskId = 
            if get_task_id x = taskId then 
                (set_task_taskStatus x TaskAccepted)::tail 
            else 
                x::(change_task tail taskId)
    in
        set_campaign_tasks campaign (change_task (get_campaign_tasks campaign) taskId) 
    end;
end;

structure Contract = 
struct

(* Context -> (SCValue list) -> (SCValue OptionErr)*)
fun constructor context params = 
    let
        (* (SCValue list) -> (SCValue OptionErr) *)
        fun validate_params params = 
            let
                val customer_addr = scvalue_to_int ((List.nth params 0));
                val customer_name = scvalue_to_string ((List.nth params 1));
                val supplier_addr = scvalue_to_int ((List.nth params 2));
                val supplier_name = scvalue_to_string ((List.nth params 3));
                val agreement_details = scvalue_to_string ((List.nth params 4));
                val bank_addr = scvalue_to_int ((List.nth params 5));
            in 
                if (((List.length params) = 6) andalso
                ((Option.isSome customer_addr) andalso ((Option.isSome customer_name) andalso
                ((Option.isSome supplier_name) andalso ((Option.isSome supplier_addr) andalso 
                ((Option.isSome agreement_details) andalso (Option.isSome bank_addr))))))) then
                    SOME (SCBool True)
                else
                    NONE "Parse params error."                                                    
            end;

        (* (SCValue list) -> (SCValue OptionErr) *)
        fun create params campaign =
            let
                val customer_addr       = (Option.valOf (scvalue_to_int    ((List.nth params 0))));
                val customer_name       = (Option.valOf (scvalue_to_string ((List.nth params 1))));
                val supplier_addr       = (Option.valOf (scvalue_to_int    ((List.nth params 2))));
                val supplier_name       = (Option.valOf (scvalue_to_string ((List.nth params 3))));
                val agreement_details   = (Option.valOf (scvalue_to_string ((List.nth params 4))));
                val bank_addr           = (Option.valOf (scvalue_to_int    ((List.nth params 5))));
            in 
                if (get_context_msgSender context) = customer_addr then
                    SOME (SCCampaign (Campaign (Agreement WaitingSupplier (Person customer_addr customer_name) (Person supplier_addr supplier_name) (AgreementDetails agreement_details bank_addr)) [] WaitingCustomer [] PhaseAgreement bank_addr []))
                else
                    NONE ( "Only customer allowed to create agreement")
            end;
        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            create params context
        else
            validated
    end; 

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getAgreement context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val customer = get_agreement_customer (get_campaign_agreement campaign);
        val supplier = get_agreement_supplier (get_campaign_agreement campaign)
    in 
        if ((sender = (get_person_addr supplier)) orelse (sender = (get_person_addr customer))) then
            RET (SCCampaign campaign) (SCAgreement (get_campaign_agreement campaign))
        else
            NONE ( "Only customer or supplier allowed to view agreement")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun rejectAgreement context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val agreement = (get_campaign_agreement campaign);
    in 
        if sender = (get_person_addr( get_agreement_supplier agreement))then
            if (get_campaign_phase campaign) = PhaseAgreement then
                if (get_agreement_negotiation agreement) = WaitingSupplier then
                    SOME (SCCampaign (set_campaign_agreement campaign (set_agreement_negotiation agreement WaitingCustomer)))
                else
                    NONE ( "Supplier is not allowed to reject at this point")
            else 
                NONE ( "Supplier is not allowed to reject at this point")
        else
            NONE ( "Only supplier allowed to reject agreement")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun approveAgreement context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val agreement = (get_campaign_agreement campaign);
    in 
        if sender = (get_person_addr((get_agreement_supplier agreement)))then
            if (get_campaign_phase campaign) = PhaseAgreement then
                if (get_agreement_negotiation agreement) = WaitingSupplier then 
                    SOME (SCCampaign (set_campaign_agreement (set_campaign_phase campaign PhaseTasks) (set_agreement_negotiation agreement NegotiationApproved)))
                else
                    NONE ( "Supplier is not allowed to approve at this point")
            else 
                NONE ( "Supplier is not allowed to approve at this point")
        else
            NONE ( "Only supplier allowed to approve agreement")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun changeAgreementDetails context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val agreement = (get_campaign_agreement campaign);
        val details = (Option.valOf (scvalue_to_string (List.nth params 0))); 
        val bankAddr = (Option.valOf (scvalue_to_int (List.nth params 1))); 
    in 
        if sender = (get_person_addr((get_agreement_customer agreement)))then
            if (get_campaign_phase campaign) = PhaseAgreement then
                if (get_agreement_negotiation agreement) = WaitingCustomer then
                    SOME (SCCampaign (set_campaign_agreement campaign (set_agreement_details (set_agreement_negotiation agreement WaitingSupplier) (AgreementDetails details bankAddr))))
                else 
                    NONE ( "Customer is not allowed to change details at this point")
            else 
                NONE ( "Customer is not allowed to change details at this point")
        else
            NONE ( "Only customer allowed to change details")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getPriceChangeWithNumber context params campaign = 
    let
    fun validate_params params = 
        let
          val index = (scvalue_to_int (List.nth params 0));
        in
          if (List.length params) = 1 then
            if (Option.isSome index) then
                 SOME (SCBool True)
            else
                NONE "Parse param error: index incorrect type."
          else
            NONE "Wrong number of params"
        end
        
        fun getPriceChange context params campaign = 
            let
        val sender = (get_context_msgSender context);
        val index = Option.valOf(scvalue_to_int (List.nth params 0));
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (List.length (get_campaign_priceChanges campaign)) > 0 then
                RET (SCCampaign campaign) (SCPriceChange (ContractPrivate.get_element_with_number (get_campaign_priceChanges campaign) index)) 
            else 
                NONE ( "No price changes")
        else
            NONE ( "Only customer allowed to view PriceChange")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            getPriceChange context params campaign
        else
            validated
    end; 

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getPriceChangesLength context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                RET (SCCampaign campaign) (SCInt ((List.length (get_campaign_priceChanges campaign)-1)))
            else 
                NONE ( "No price changes")
        else
            NONE ( "Only customer allowed to view count of PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun rejectPrice context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = WaitingCustomer then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ( (ContractPrivate.cut_last_element (get_campaign_priceChanges campaign)) @ [(set_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign)) NegotiationRejected)] )))
                    else 
                        NONE ( "Customer is not allowed to reject at this point")
                else 
                    NONE ( "No price changes")
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to reject PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun approvePrice context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = WaitingCustomer then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ( (ContractPrivate.cut_last_element (get_campaign_priceChanges campaign)) @ [(set_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign)) NegotiationApproved)])))
                    else 
                        NONE ( "Customer is not allowed to approve at this point")
                else 
                    NONE ( "No price changes")
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to approve PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun declinePrice context params campaign = 
    let
        val sender = (get_context_msgSender context);
    in 
        if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = WaitingCustomer then 
                        SOME (SCCampaign (set_campaign_phase campaign PhaseDeclined))
                    else 
                        NONE ( "Customer is not allowed to decline at this point")
                else 
                    NONE ( "No price changes")
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to decline PriceChange")
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun createPriceChange context params campaign = 
    let
    fun validate_params params = 
        let
            val price =       (scvalue_to_int( List.nth params 0));
            val negotiation = (scValue_to_negotiation( List.nth params 1));
            val startTime =   (scvalue_to_int( List.nth params 2));
        in
          if (List.length params) = 3 then
            if (Option.isSome price) then
                if (Option.valOf price) > 0 then
                    if (Option.isSome negotiation) then
                        if (Option.isSome startTime) then
                            if (Option.valOf startTime) > 0 then
                                SOME (SCBool True)
                            else
                                NONE "Start Time must be more than 0."
                        else
                            NONE "Parse param error: startTime incorrect type."
                    else
                        NONE "Parse param error: negotiation incorrect type."
                else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: price incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun create context params campaign = 
    let
        val sender = (get_context_msgSender context);

        val price = Option.valOf (scvalue_to_int( List.nth params 0));
        val negotiation = Option.valOf(scValue_to_negotiation( List.nth params 1));
        val startTime = Option.valOf(scvalue_to_int( List.nth params 2));
    in 
        if sender = get_person_addr (get_agreement_supplier (get_campaign_agreement campaign)) then
            if (get_campaign_phase campaign) = PhaseTasks then
                if (List.length (get_campaign_priceChanges campaign)) > 0 then 
                    if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = NegotiationApproved then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [ (PriceChange price negotiation startTime )] )))
                    else if (get_priceChange_negotiation (ContractPrivate.get_last_element (get_campaign_priceChanges campaign))) = NegotiationRejected then 
                        SOME (SCCampaign (set_campaign_priceChanges campaign ((get_campaign_priceChanges campaign) @ [ (PriceChange price negotiation startTime )] )))
                    else 
                        NONE ("The last change of price should be approved or rejected before adding another priceChange")
                else 
                    SOME (SCCampaign (set_campaign_priceChanges campaign [(PriceChange price negotiation startTime )] ))
            else
                NONE ("Phase is not PhaseTasks")
        else
            NONE ("Only supplier allowed to create PriceChange")
    end
            
        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            create context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun getTask context params campaign = 
    let
    fun validate_params params = 
        let
            val taskId = (scvalue_to_int (List.nth params 0));
        in 
            if (Option.isSome taskId) then 
                if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                    SOME (SCBool True)
                else
                    NONE "Task does not exist."
            else
                NONE "Parse param error: taskId. Incorrect type."
        end;
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun action context params campaign = 
        let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf (scvalue_to_int (List.hd params));
        val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
        val agreement = (get_campaign_agreement campaign);
    in 
        if ((sender = get_person_addr (get_agreement_supplier agreement )) orelse (sender = get_person_addr (get_agreement_customer agreement))) then
            RET (SCCampaign campaign) (SCTask task)
        else
            NONE ( "Only supplier or customer allowed to get task")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun approveTask context params campaign = 
    let
    fun validate_params params = 
        let
          val taskId =  (scvalue_to_int (List.hd params));
        in
          if (List.length params) = 1 then
            if (Option.isSome taskId) then
                if (Option.valOf taskId) > 0 then
                    SOME (SCBool True)
                 else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: taskId incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun approve context params campaign = 
        let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf (scvalue_to_int (List.hd params));
    in 
        if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
            if sender = get_person_addr (get_agreement_supplier (get_campaign_agreement campaign)) then
                if (get_campaign_phase campaign) = PhaseTasks then
                    if get_task_negotiation (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId))) <> NegotiationRejected then
                        SOME (SCCampaign (ContractPrivate.p_approve_task campaign taskId))
                    else
                        NONE ( "Task was rejected")
                else
                    NONE ( "Phase is not PhaseTasks")
            else
                NONE ( "Only supplier allowed to approve")
        else
            NONE ( "Task does not exists.")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            approve context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun rejectTask context params campaign = 
    let
    fun validate_params params = 
        let
          val taskId = scvalue_to_int (List.hd params);
        in
          if (List.length params) = 1 then
            if (Option.isSome taskId) then
                if (Option.valOf taskId) > 0 then
                    SOME (SCBool True)
                 else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: taskId incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun reject context params campaign = 
    let
        val sender = get_context_msgSender context;
        val taskId = Option.valOf (scvalue_to_int (List.hd params));
    in 
        if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
            if sender = get_person_addr (get_agreement_supplier (get_campaign_agreement campaign)) then
                if (get_campaign_phase campaign) = PhaseTasks then
                    if get_task_negotiation (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId))) <> NegotiationApproved then
                            SOME (SCCampaign (ContractPrivate.p_reject_task campaign taskId))
                    else
                        NONE ( "Task was approved")
                else
                    NONE ( "Phase is not PhaseTasks")
            else
                NONE ( "Only supplier allowed to reject Task")
        else
            NONE ( "Task does not exists.")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            reject context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun acceptTask context params campaign = 
    let
        fun validate_params params = 
            let
            val taskId = (scvalue_to_int (List.hd params));
            in
            if (List.length params) = 1 then
                if (Option.isSome taskId) then
                    if (Option.valOf taskId) > 0 then
                        SOME (SCBool True)
                    else
                        NONE "Price must be more than 0."
                else
                    NONE "Parse param error: taskId incorrect type."
            else
                NONE "Wrong number of params"
            end;
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun accept context params campaign = 
        let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf(scvalue_to_int (List.hd params));
        in 
            if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
                if sender = get_person_addr (get_task_worker (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)))) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if get_task_negotiation (Option.valOf (get (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId))) = NegotiationApproved then
                            SOME (SCCampaign (ContractPrivate.p_accept_task campaign taskId))
                        else
                            NONE ( "Task is not approved yet.")
                    else
                        NONE ( "Phase is not PhaseTasks")
                else
                    NONE ( "Only worker allowed to accept Task")
            else
                NONE ( "Task does not exists.")
        end;

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            accept context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun removeTask context params campaign = 
    let
    fun validate_params params = 
        let
          val taskId = (scvalue_to_int (List.hd params));
        in
          if (List.length params) = 1 then
            if (Option.isSome taskId) then
                if (Option.valOf taskId) > 0 then
                    SOME (SCBool True)
                 else
                    NONE "Price must be more than 0."
            else
                NONE "Parse param error: taskId incorrect type."
          else
            NONE "Wrong number of params"
        end
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun remove context params campaign = 
    let
        val sender = (get_context_msgSender context);
        val taskId = Option.valOf(scvalue_to_int (List.hd params));
    in 
        if Option.isNone (get_err (ContractPrivate.p_get_task_by_id (get_campaign_tasks campaign) taskId)) then
            if sender = get_person_addr (get_agreement_customer (get_campaign_agreement campaign)) then
                if (get_campaign_phase campaign) = PhaseTasks then
                        SOME (SCCampaign (ContractPrivate.p_remove_task_from_Campaign campaign taskId) )
                else
                    NONE ( "Phase is not PhaseTasks")
            else
                NONE ( "Only customer allowed to remove Task")
        else
            NONE ( "Task does not exists.")
    end

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            remove context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun addTask context params campaign = 
     let
        fun validate_params params = 
        let
          val taskId =         (scvalue_to_int (List.nth params 0));
          val negotiation =    (scValue_to_negotiation (List.nth params 1));
          val captain_addr =   (scvalue_to_int (List.nth params 2));
          val captain_name =   (scvalue_to_string (List.nth params 3));
          val worker_addr =    (scvalue_to_int (List.nth params 4));
          val worker_name =    (scvalue_to_string (List.nth params 5));
          val expectedGas =    (scvalue_to_int (List.nth params 6));
          val requestedGas =   (scvalue_to_int (List.nth params 7));
          val suppliedGas =    (scvalue_to_int (List.nth params 8));
          val totalGas =       (scvalue_to_int (List.nth params 9));
          val requestTime =    (scvalue_to_int (List.nth params 10));
          val suppliedTime =   (scvalue_to_int (List.nth params 11));
          val completionTime = (scvalue_to_int (List.nth params 12));
          val paymentTime =    (scvalue_to_int (List.nth params 13));
          val taskStatus =     (scValue_to_taskStatus (List.nth params 14));
          val paymentType =    (scValue_to_paymentType (List.nth params 15));
        in
            if (((List.length params) = 16) andalso
            ((Option.isSome taskId) andalso ((Option.isSome negotiation) andalso
            ((Option.isSome captain_name) andalso ((Option.isSome captain_addr) andalso
            ((Option.isSome worker_name) andalso ((Option.isSome worker_addr) andalso
            ((Option.isSome expectedGas) andalso ((Option.isSome requestedGas) andalso
            ((Option.isSome suppliedGas) andalso ((Option.isSome totalGas) andalso
            ((Option.isSome requestTime) andalso ((Option.isSome suppliedTime) andalso
            ((Option.isSome completionTime) andalso ((Option.isSome paymentTime) andalso
            ((Option.isSome taskStatus) andalso (Option.isSome paymentType))))))))))))))))) then
                SOME (SCBool True)
            else
                NONE "Parse param error"
        end;
        
    (* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
    fun add context params campaign = 
        let
            val sender = (get_context_msgSender context);

          val taskId =         Option.valOf (scvalue_to_int (List.nth params 0));
          val negotiation =    Option.valOf (scValue_to_negotiation (List.nth params 1));
          val captain_addr =   Option.valOf (scvalue_to_int (List.nth params 2));
          val captain_name =   Option.valOf (scvalue_to_string (List.nth params 3));
          val worker_addr =    Option.valOf (scvalue_to_int (List.nth params 4));
          val worker_name =    Option.valOf (scvalue_to_string (List.nth params 5));
          val expectedGas =    Option.valOf (scvalue_to_int (List.nth params 6));
          val requestedGas =   Option.valOf (scvalue_to_int (List.nth params 7));
          val suppliedGas =    Option.valOf (scvalue_to_int (List.nth params 8));
          val totalGas =       Option.valOf (scvalue_to_int (List.nth params 9));
          val requestTime =    Option.valOf (scvalue_to_int (List.nth params 10));
          val suppliedTime =   Option.valOf (scvalue_to_int (List.nth params 11));
          val completionTime = Option.valOf (scvalue_to_int (List.nth params 12));
          val paymentTime =    Option.valOf (scvalue_to_int (List.nth params 13));
          val taskStatus =     Option.valOf (scValue_to_taskStatus (List.nth params 14));
          val paymentType =     Option.valOf (scValue_to_paymentType (List.nth params 15));
        in 
        if sender = get_person_addr( get_agreement_customer (get_campaign_agreement campaign) )then
            if (get_campaign_phase campaign) = PhaseTasks then
                    SOME (SCCampaign (ContractPrivate.p_add_task_in_Campaign campaign (Task taskId negotiation (Person captain_addr captain_name) (Person worker_addr worker_name) expectedGas requestedGas suppliedGas totalGas requestTime suppliedTime completionTime paymentTime taskStatus paymentType) ))
            else
                NONE ( "Phase is not PhaseTasks")
        else
            NONE ( "Only customer allowed to add Task")
    end;

        val validated = (validate_params params);
    in
        if Option.isNone (get_err validated) then 
            add context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun readyToPerformTask context params campaign = 
    let 
        val sender = (get_context_msgSender context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
            in 
                if Option.isSome taskId then 
                    if Option.isSome(ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        SOME (SCBool True)
                    else
                        NONE ("Task does not exist.")
                else
                    NONE ("Parse param error: TaskID. Incorrect type.")
            end;

        fun action context params campaign =
            let 
                val taskId = (Option.valOf(scvalue_to_int (List.nth params 0)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in 
                if sender = (get_person_addr (get_task_worker task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = TaskAccepted then
                            SOME (SCCampaign (ContractPrivate.p_update_Campaign_tasks campaign (set_task_taskStatus task TaskReadyToPerform) taskId))
                        else 
                            NONE ( "Task is not accepted yet.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only worker allowed to do this action.")
            end;

        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun requestGas context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val requestTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
                val amount = (scvalue_to_int (List.nth params 1));
                val paymentTime = (scvalue_to_int (List.nth params 2));

                val priceChange = (ContractPrivate.p_get_last_price_change campaign);
            in 
                if Option.isSome taskId then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId )) then 
                        if Option.isSome amount then
                            if (Option.valOf amount) > 0 then
                                if Option.isSome paymentTime then 
                                    if (Option.valOf paymentTime) > 0 then
                                        if ( (get_task_paymentType (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId )))) = Pre ) andalso (Option.isNone (ContractPrivate.calculateLastPrice campaign)) then
                                            NONE "No approved prices."
                                        else
                                            SOME (SCBool True)
                                    else
                                        NONE "Payment time must be more than 0."
                                else
                                    NONE  "Parse param error: PaymentTime. Incorrect type."
                            else
                                NONE "Amount must be more than 0."
                        else
                            NONE "Parse param error: Amount. Incorrect type."
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: TaskId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val amount = (Option.valOf (scvalue_to_int (List.nth params 1)));
                val paymentTime = (Option.valOf (scvalue_to_int (List.nth params 2)));

                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
                val price = Option.valOf (ContractPrivate.calculateLastPrice campaign);

                (* Task -> Task *)
                fun update_info task = (set_task_requestTime (set_task_requestedGas (set_task_totalGas (set_task_taskStatus task GasRequested) ((get_task_totalGas task) + amount)) amount) requestTime)
                (* Campaign -> Campaign *)
                fun add_payment_order campaign = 
                    if (get_task_paymentType task = Pre) then 
                        set_campaign_paymentOrders campaign (( get_campaign_paymentOrders campaign) @ [(PaymentOrder (amount*price) paymentTime 0 taskId WaitingForPayment True)])
                    else
                        campaign;
                                    
            in
                if sender = (get_person_addr (get_task_captain task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task = TaskReadyToPerform) then
                            SOME (SCCampaign 
                                (ContractPrivate.p_update_Campaign_tasks (add_payment_order campaign) (update_info task) taskId)
                            )
                        else 
                            NONE ("Task is not ready to be performed.")
                    else
                        NONE ("Action is not allowed at this point.")
                else 
                    NONE ("Only captain allowed to do this action.")
            end;
        val validated = (validate_params params campaign);
        
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun paymentCompleted context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val requestTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val paymentId = (scvalue_to_int (List.nth params 0));
            in 
                if (Option.isSome paymentId) then 
                    if (Option.isSome (ContractPrivate.p_get_paymentOrder_by_id campaign ( Option.valOf paymentId))) then
                        if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (get_PaymentOrder_taskId (Option.valOf (ContractPrivate.p_get_paymentOrder_by_id campaign ( Option.valOf paymentId))))) then
                            SOME (SCBool True)
                        else
                            NONE "Task does not exist."
                    else 
                        NONE "Payment order does not exist."
                else
                    NONE "Parse param error: PaymentId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val paymentId = (Option.valOf(scvalue_to_int (List.nth params 0)));
                val paymentOrder = (Option.valOf(ContractPrivate.p_get_paymentOrder_by_id campaign paymentId));
                val taskId = (get_PaymentOrder_taskId paymentOrder);
                val task = (Option.valOf (ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in
                if sender = (get_campaign_bankAddress campaign) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if ((get_task_taskStatus task) = Confirmed) then
                            SOME (SCCampaign 
                                (ContractPrivate.p_update_Campaign_tasks (ContractPrivate.p_update_paymentOrder_by_id campaign (set_PaymentOrder_paymentStatus paymentOrder PaymentCompleted) paymentId) (set_task_taskStatus task TaskCompleted) taskId)
                            )
                        else
                            SOME (SCCampaign 
                                (ContractPrivate.p_update_paymentOrder_by_id campaign (set_PaymentOrder_paymentStatus paymentOrder PaymentCompleted) paymentId) 
                            )
                    else
                        NONE ( "Action is not allowed at this point.")
                else 
                    NONE ( "Only bank allowed to do this action.")
            end;
        val validated = (validate_params params campaign);
        
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun performTask context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val requestTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
            in 
                if (Option.isSome taskId) then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        SOME (SCBool True)
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: taskId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in
                if sender = (get_person_addr (get_task_worker task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = GasRequested then
                            SOME (SCCampaign (ContractPrivate.p_update_Campaign_tasks campaign (set_task_taskStatus task Performing) taskId))
                        else 
                            NONE ( "No gas requests.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only worker allowed to do this action.")
            end;
        
        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun taskCompleted context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val suppliedTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
                val suppliedGas = (scvalue_to_int (List.nth params 1));

            in 
                if (Option.isSome taskId) then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        SOME (SCBool True)
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: taskId. Incorrect type."
            end;

        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val suppliedGas = (Option.valOf (scvalue_to_int (List.nth params 1)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
            in
                if sender = (get_person_addr (get_task_worker task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = Performing then
                            SOME (SCCampaign (ContractPrivate.p_update_Campaign_tasks campaign 
                                                (set_task_taskStatus 
                                                    (set_task_suppliedGas 
                                                        (set_task_suppliedTime task suppliedTime) 
                                                        suppliedGas) 
                                                    TaskCompleted) 
                                                taskId))
                        else 
                            NONE (  "No one is perfroming the task.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only worker allowed to do this action.")
            end;
        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

(* Context -> (SCValue list) -> Campaign -> (SCValue OptionErr)  *)
fun confirmTask context params campaign = 
    let 
        val sender = (get_context_msgSender context);
        val suppliedTime = (get_context_blockNum context);

        fun validate_params params campaign = 
            let
                val taskId = (scvalue_to_int (List.nth params 0));
            in 
                if (Option.isSome taskId) then 
                    if Option.isSome (ContractPrivate.p_get_campaign_task_by_task_id campaign (Option.valOf taskId)) then
                        if (Option.isSome (ContractPrivate.calculateLastPrice campaign)) then 
                            SOME (SCBool True)
                        else
                            NONE "No approved prices."
                    else
                        NONE "Task does not exist."
                else
                    NONE "Parse param error: taskId. Incorrect type."
            end;
        
        fun action context params campaign = 
            let
                val taskId = (Option.valOf (scvalue_to_int (List.nth params 0)));
                val task = (Option.valOf(ContractPrivate.p_get_campaign_task_by_task_id campaign taskId));
                val direction = 
                    if (get_task_suppliedGas task) < (get_task_totalGas task) then
                        if (get_task_paymentType task) = Pre then
                            False
                        else
                            True
                    else
                        True;

                fun make_payment_order task =
                    let 
                        val suppliedGas = (get_task_suppliedGas task);
                        val totalGas = (get_task_totalGas task);
                        val price = Option.valOf (ContractPrivate.calculateLastPrice campaign);
                    in
                        if (get_task_paymentType task) = Pre then 
                            if suppliedGas < totalGas then
                                (PaymentOrder (( totalGas - suppliedGas )* price) 0 0 taskId WaitingForPayment direction)
                            else
                                (PaymentOrder (( suppliedGas - totalGas ) * price) 0 0 taskId WaitingForPayment direction)
                        else if (get_task_paymentType task) = Post then 
                            (PaymentOrder ( suppliedGas * price) 0 0 taskId WaitingForPayment direction)
                        else 
                            (PaymentOrder ( suppliedGas * price) (get_task_paymentTime task) 0 taskId WaitingForPayment direction)
                    end;

                (* Campaign -> Campaign *)
                fun add_payment_order campaign order = 
                    set_campaign_paymentOrders campaign (( get_campaign_paymentOrders campaign) @ [order])
            in
                if sender = (get_person_addr (get_task_captain task)) then
                    if (get_campaign_phase campaign) = PhaseTasks then
                        if (get_task_taskStatus task) = TaskCompleted then
                            SOME (SCCampaign (add_payment_order 
                                                (ContractPrivate.p_update_Campaign_tasks campaign (set_task_taskStatus task Confirmed) taskId) 
                                                (make_payment_order task )))
                        else 
                            NONE ( "No gas requests.")
                    else
                        NONE ( "Action is not allowed at this point")
                else 
                    NONE ( "Only captain allowed to do this action.")
            end;

        val validated = (validate_params params campaign);
    in
        if Option.isNone (get_err validated) then 
            action context params campaign
        else
            validated
    end;

end;structure RuntimePrivate =
struct
    (* int -> (Word8.word list) -> (SCValue list) -> (SCValue OptionErr)*)
    fun execute f context params campaign =
        case f of
        2 => Contract.getAgreement context params campaign
        | 3 => Contract.rejectAgreement context params campaign (* *)
        | 4 => Contract.approveAgreement context params campaign
        | 5 => Contract.changeAgreementDetails context params campaign
        | 6 => Contract.getPriceChangeWithNumber context params campaign
        | 7 => Contract.getPriceChangesLength context params campaign
        | 8 => Contract.rejectPrice context params campaign
        | 9 => Contract.approvePrice context params campaign
        | 10 => Contract.declinePrice context params campaign
        | 11 => Contract.createPriceChange context params campaign
        | 12 => Contract.getTask context params campaign
        | 13 => Contract.approveTask context params campaign
        | 14 => Contract.rejectTask context params campaign
        | 15 => Contract.acceptTask context params campaign
        | 16 => Contract.removeTask context params campaign
        | 17 => Contract.addTask context params campaign
        | 18 => Contract.readyToPerformTask context params campaign
        | 19 => Contract.requestGas context params campaign
        | 20 => Contract.paymentCompleted context params campaign
        | 21 => Contract.performTask context params campaign
        | 22 => Contract.taskCompleted context params campaign
        | 23 => Contract.confirmTask context params campaign
        | n => NONE "The function doesn't exist";
end;

structure Runtime =
struct
    fun call f context params =
    let
        val storage = (get_context_storage context);

        (*(Word8Word list) -> Campaign option*)
        fun deserialize storage =
            if storage = [] then
                None
            else
                scValue_to_campaign (decodeValue storage);

        val campaign = deserialize storage;

        val optionCampaign =
            if Option.isSome campaign then
                if f <> 1 then
                    (RuntimePrivate.execute f context params (Option.valOf campaign))
                else
                    NONE "Storage is not empty"
            else if f = 1 then
                Contract.constructor context params
            else
                NONE "Error deserialization";
    in
        if Option.isNone (get_err optionCampaign) then
            if Option.isSome (get optionCampaign) then
                SOME (encodeValue (Option.valOf ( get optionCampaign )))
            else
                RET (encodeValue (Option.valOf ( get_ret_state optionCampaign ))) (encodeValue (Option.valOf ( get_ret_val optionCampaign )))
        else
            NONE (Option.valOf (get_err optionCampaign))
    end;
end;
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
				(*
				5 ролей - Customer, Supplier, Worker, Captain, Bank
				 Customer - авиакомпания
				 Supplier - топливня компнаия
				 Worker - работник Supplier
				 Captain - работник Customer
				 Bank - банк, логично

				Ещё полезные вещи
				campaign - состояние контракта
				*)
				let
        	val storage = [];
        	val customer = Context 1337 10217 storage;
        	val init_params = [ SCInt 1337, SCString "aviacompany", SCInt 1338, SCString "fuelcompany", SCString "This very simple agreement", SCInt 1339 ];
        	val result_init = Runtime.call 1 customer init_params; (*передаем campaign*)

					val result_getting_agreement_by_cus = Role.choose_role 1 1337 10218 1 result_init [];
					(* val _ = print ( reformat_agreement_to_string result_getting_agreement_by_cus) *)

					val result_getting_agreement_by_sup = Role.choose_role 2 1338 10219 1 result_init [];
					(* val _ = print ( reformat_agreement_to_string result_getting_agreement_by_sup) *)

					val result_reject_agreement = Role.choose_role 2 1338 10220 2 result_init [];
					(* val _ = print (reformat_campaign_to_string result_reject_agreement) *)

					val result_change_agreement_details = Role.choose_role 1 1337 10221 2 result_reject_agreement [SCString "Buy all hamburgers", SCInt 1339];
					(* val _ = print (reformat_campaign_to_string result_change_agreement_details) *)

					val result_approve_agreement = Role.choose_role 2 1338 10222 3 result_change_agreement_details [];
				(*	val _ = print (reformat_campaign_to_string result_approve_agreement) *)


					(* срач из-за цены *)
					val price_params = [SCInt 228, SCNegotiation WaitingCustomer, SCInt 666]
					val result_create_price_change = Role.choose_role 2 1338 10223 4 result_approve_agreement price_params;
					(* val _ = print (reformat_campaign_to_string result_create_price_change); *)
					val result_get_price_changes_length = Role.choose_role 1 1337 10224 4 result_create_price_change [];
					(* val result_reject_price_change = Role.choose_role 1 1337 10225 5 result_create_price_change []; *)
					val result_approve_price_change = Role.choose_role 1 1337 10225 6 result_create_price_change [];
				(*	val _ = print (reformat_campaign_to_string result_approve_price_change); *)
					(* val result_decline_price_change = Role.choose_role 1 1337 10225 7 result_create_price_change []; *)

					(* Add Task *)
					val addtaskparams = [SCInt 228, SCNegotiation WaitingSupplier,
					SCInt 1340, SCString "Aero",
					SCInt 1341, SCString "Oily",
					SCInt 1000, SCInt 1000, SCInt 1000, SCInt 1000,
					SCInt 1, SCInt 10, SCInt 11, SCInt 5,
					SCTaskStatus TaskNotAccepted, SCPaymentType Pre]; (*TODO list*)
					val result_task_add = Role.choose_role 1 1337 10226 10 result_approve_price_change addtaskparams;
					val _ = print(reformat_campaign_to_string result_task_add);
					val result_getting_tasks_by_sup = Role.choose_role 2 1338 10227 5 result_task_add [SCInt 228];
					val _ = print(reformat_task_to_string result_getting_tasks_by_sup);
					val result_approve_task = Role.choose_role 2 1338 10228 6 result_task_add [SCInt 228];
					val _ = print(reformat_campaign_to_string result_approve_task);
					(* val result_reject_task = Role.choose_role 2 1338 10228 result_getting_tasks_by_sup 228;*)

					(*БЛОК 2. Sup и Cus уходят бухать*)

					val result_worker_accept_task = Role.choose_role 3 1341 10229 2 result_approve_task [SCInt 228];
					val _ = print(reformat_campaign_to_string result_worker_accept_task);

					val result_worker_ready_to_perform_task = Role.choose_role 3 1341 10230 1 result_worker_accept_task [SCInt 228];
					val _ = print(reformat_campaign_to_string result_worker_ready_to_perform_task);

					val result_capitan_request_gas = Role.choose_role 4 1340 10231 1 result_worker_ready_to_perform_task [SCInt 228, SCInt 1000, SCInt 5];
					val _ = print(reformat_campaign_to_string result_capitan_request_gas);

					val result_worker_perform_task = Role.choose_role 3 1341 10232 3 result_capitan_request_gas [SCInt 228];
					val _ = print(reformat_campaign_to_string result_worker_perform_task);

					val result_worker_task_complete = Role.choose_role 3 1341 10233 4 result_worker_perform_task [SCInt 228, SCInt 1000];
					val _ = print(reformat_campaign_to_string result_worker_task_complete);

					val result_capitan_confirm_task = Role.choose_role 4 1340 10234 2 result_worker_task_complete [SCInt 228];
					val _ = print(reformat_campaign_to_string result_capitan_confirm_task);

					val result_payment_completed = Role.choose_role 5 1339 10235 1 result_capitan_confirm_task [SCInt 1];


				in
				if Option.isNone (get_err result_payment_completed) then
						print(campaign_toPrettyString ((Option.valOf( scValue_to_campaign( decodeValue(Option.valOf(get ( result_payment_completed ))))))))
				 else
						 print(Option.valOf(get_err ( result_payment_completed )))
				end;

main ();
