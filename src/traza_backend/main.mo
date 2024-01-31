import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Prim "mo:prim";
import Prelude "mo:base/Prelude";
import Map "mo:base/HashMap";
import Hash "mo:base/Hash";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Nat32 "mo:base/Nat32";


actor HttpCounter {

  type Process = {
    idProcess: Text;
    descProcess: Text;
  };
  stable var processes : [(Nat, Process)] = [];
  let processMap = Map.fromIter<Nat, Process>(processes.vals(),10,Nat.equal , Hash.hash);
  
    type SubProcess = {
    idProcess: Text;
    idSubProcess: Text;
    descSubProcess: Text;
  };
  stable var subProcesses : [(Nat, SubProcess)] = [];
  let subProcessMap = Map.fromIter<Nat, SubProcess>(subProcesses.vals(),10,Nat.equal , Hash.hash);
    type Hecta = {
    idHectarea: Text;
    descHecta: Text;
  };
  stable var hectas : [(Nat, Hecta)] = [];
  let hectaMap = Map.fromIter<Nat, Hecta>(hectas.vals(),10,Nat.equal , Hash.hash);
    type Lote = {
    idLote: Text;
    descLote: Text;
  };
  stable var lotes : [(Nat, Lote)] = [];
  let lotesMap = Map.fromIter<Nat, Lote>(lotes.vals(),10,Nat.equal , Hash.hash);
  type Product = {
    idProduct: Text;
    descProduct: Text;
  };
  stable var products : [(Nat, Product)] = [];
  let productMap = Map.fromIter<Nat, Product>(products.vals(),10,Nat.equal , Hash.hash);
  type Transaction = {
    idTransaction : Text;
    idTransactionPrev : Text;
    idTransactionPost : Text;
    idProcess : Text;
    idSubprocess : Text;
    idHecta : Text;
    idLote : Text;
    idProduct : Text;
    gln : Text;
    gtin : Text;
    register_date : Text;
    resp_id : Text;
    weight : Text;
    amount : Text;
    hash_id_transaction : Nat32;
  };
stable var transactions : [(Nat, Transaction)] = [];
  let transactionsMap = Map.fromIter<Nat, Transaction>(transactions.vals(),10,Nat.equal , Hash.hash);
  type StreamingCallbackHttpResponse = {
    body: Blob;
    token: ?Token;
  };

  type Token = {
    // Add whatever fields you'd like
    arbitrary_data: Text;
  };

  type CallbackStrategy = {
    callback: shared query (Token) -> async StreamingCallbackHttpResponse;
    token: Token;
  };

  type StreamingStrategy =  {
    #Callback: CallbackStrategy;
  };

  type HeaderField = (Text, Text);

  type HttpResponse = {
    status_code: Nat16;
    headers: [HeaderField];
    body: Blob;
    streaming_strategy: ?StreamingStrategy;
    upgrade: ?Bool;
  };

  type HttpRequest = {
    method: Text;
    url: Text;
    headers: [HeaderField];
    body: Blob;
  };


  stable var counter = 0;

  public query func http_request(req : HttpRequest) : async HttpResponse {
    let requr = Iter.toArray(Text.split(req.url,#char '?'))[0];
    let cnt = Iter.toArray(Text.split(req.url,#char '?')).size();
    switch (req.method,requr, cnt ) {
      case ("GET", "/proceso", 1) {
        var resp:Text =  "{ \"processs\" : [ ";
        var aux = 0; 
        for (val in processMap.vals()){
          resp:= resp # "{ \"idProcess\":\"" # val.idProcess # "\" , \"descProcess : \"" # val.descProcess # "\"";
          aux := aux + 1;
          if (aux == processMap.size()){
            resp:= resp # "}";
            }
            else {
              resp:= resp # "},";
            };
        };
        resp:= resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
        }
      
      };
      case ("GET", "/proceso", 2) {
        var resp:Text =  "{ \"process\" : [ ";
        let processId =  Iter.toArray(Text.split(req.url,#char '='))[1];
        for (val in processMap.vals()){
          if (val.idProcess == processId){
            resp:= resp # "{ \"idProcess\":\"" # val.idProcess # "\" , \"descProcess : \"" # val.descProcess # "\"";
          }
        };
        resp := resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
      }};
      case ("POST", "/proceso", 2) {{
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8("{\n \"counter\" : "# Nat.toText(counter)# " }");
        streaming_strategy = null;
        upgrade = ?true;
      }};
            case ("GET", "/subproceso", 1) {
        var resp:Text =  "{ \"subproces\" : [ ";
        var aux = 0; 
        for (val in subProcessMap.vals()){
          resp:= resp # "{ \"idProcess\":\"" # val.idProcess # "\",\"idSubProcess\":\"" # val.idSubProcess # "\" , \"descProcess : \"" # val.descSubProcess # "\"";
          aux := aux + 1;
          if (aux == subProcessMap.size()){
            resp:= resp # "}";
            }
            else {
              resp:= resp # "},";
            };
        };
        resp:= resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
        }
      
      };
      case ("GET", "/subproceso", 2) {
        var resp:Text =  "{ \"subproccess\" : [ ";
        let subProcessId =  Iter.toArray(Text.split(req.url,#char '='))[1];
        for (val in subProcessMap.vals()){
          if (val.idSubProcess == subProcessId){
            resp:= resp # "{ \"idProcess\":\"" # val.idProcess # " \"idSubProcess\":\"" # val.idSubProcess # "\", \"descProcess\":\""  # val.descSubProcess # "\"";
          }
        };
        resp := resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
      }};
      case ("POST", "/subproceso", 2) {{
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8("{\n \"counter\" : "# Nat.toText(counter)# " }");
        streaming_strategy = null;
        upgrade = ?true;
      }};
      case ("GET", "/hecta", 1) {
        var resp:Text =  "{ \"hectarea\" : [ ";
        var aux = 0; 
        for (val in hectaMap.vals()){
          resp:= resp # "{ \"idHectarea\":\"" # val.idHectarea # "\" , \"descProcess : \"" # val.descHecta # "\"";
          aux := aux + 1;
          if (aux == hectaMap.size()){
            resp:= resp # "}";
            }
            else {
              resp:= resp # "},";
            };
        };
        resp:= resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
        }
      
      };
      case ("GET", "/hectarea", 2) {
        var resp:Text =  "{ \"hectarea\" : [ ";
        let hectareaId =  Iter.toArray(Text.split(req.url,#char '='))[1];
        for (val in hectaMap.vals()){
          if (val.idHectarea == hectareaId){
            resp:= resp # "{ \"idHectarea\":\"" # val.idHectarea # "\" , \"descHectarea : \"" # val.descHecta # "\"";
          }
        };
        resp := resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
      }};
      case ("POST", "/hectarea", 2) {{
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8("{\n \"counter\" : "# Nat.toText(counter)# " }");
        streaming_strategy = null;
        upgrade = ?true;
      }};
            case ("GET", "/lote", 1) {
        var resp:Text =  "{ \"lote\" : [ ";
        var aux = 0; 
        for (val in lotesMap.vals()){
          resp:= resp # "{ \"idLote\":\"" # val.idLote # "\" , \"descLote : \"" # val.descLote # "\"";
          aux := aux + 1;
          if (aux == lotesMap.size()){
            resp:= resp # "}";
            }
            else {
              resp:= resp # "},";
            };
        };
        resp:= resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
        }
      
      };
      case ("GET", "/lote", 2) {
        var resp:Text =  "{ \"lote\" : [ ";
        let loteId =  Iter.toArray(Text.split(req.url,#char '='))[1];
        for (val in lotesMap.vals()){
          if (val.idLote == loteId){
            resp:= resp # "{ \"idLote\":\"" # val.idLote # "\" , \"descLote : \"" # val.descLote # "\"";
          }
        };
        resp := resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
      }};
      case ("POST", "/lote", 2) {{
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8("{\n \"counter\" : "# Nat.toText(counter)# " }");
        streaming_strategy = null;
        upgrade = ?true;
      }};
            case ("GET", "/producto", 1) {
        var resp:Text =  "{ \"products\" : [ ";
        var aux = 0; 
        for (val in productMap.vals()){
          resp:= resp # "{ \"idProduct\":\"" # val.idProduct # "\" , \"descProduct : \"" # val.descProduct # "\"";
          aux := aux + 1;
          if (aux == productMap.size()){
            resp:= resp # "}";
            }
            else {
              resp:= resp # "},";
            };
        };
        resp:= resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
        }
      
      };
      case ("GET", "/producto", 2) {
        var resp:Text =  "{ \"product\" : [ ";
        let productId =  Iter.toArray(Text.split(req.url,#char '='))[1];
        for (val in productMap.vals()){
          if (val.idProduct == productId){
            resp:= resp # "{ \"idProduct\":\"" # val.idProduct # "\" , \"descProduct : \"" # val.descProduct # "\"";
          }
        };
        resp := resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
      }};
      case ("POST", "/producto", 2) {{
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8("{\n \"counter\" : "# Nat.toText(counter)# " }");
        streaming_strategy = null;
        upgrade = ?true;
      }};
            case ("GET", "/transaccion", 1) {
        var resp:Text =  "{ \"transaccion\" : [ ";
        var aux = 0; 
        for (val in transactionsMap.vals()){
          resp:= resp # "{ \"idTransaction\":\"" # val.idTransaction # "\" , \"idTransactionPrev\": \"" # val.idTransactionPrev # "\" , \"idTransactionPost\": \"" # val.idTransactionPost # "\" , \"idProcess\": \"" # val.idProcess # "\" , \"idSubprocess\": \"" # val.idSubprocess # "\" , \"idHecta\": \"" # val.idHecta # "\" , \"idLote\": \"" # val.idLote # "\" , \"idProduct\": \"" # val.idProduct # "\" , \"gln\": \"" # val.gln # "\" , \"gtin\": \"" # val.gtin # "\" , \"register_date\": \"" # val.register_date # "\" , \"resp_id\": \"" # val.resp_id # "\" , \"weight\": \"" # val.weight # "\" , \"amount\": \"" # val.amount # "\" ";
          aux := aux + 1;
          if (aux == transactionsMap.size()){
            resp:= resp # "}";
            }
            else {
              resp:= resp # "},";
            };
        };
        resp:= resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
        }
      
      };
      case ("GET", "/transaccion", 2) {
        var resp:Text =  "{ \"transaccion\" : [ ";
        let transactionId =  Iter.toArray(Text.split(req.url,#char '='))[1];
        for (val in transactionsMap.vals()){
          if (val.idTransaction == transactionId){
             resp:= resp # "{ \"idTransaction\":\"" # val.idTransaction # "\" , \"idTransactionPrev\": \"" # val.idTransactionPrev # "\" , \"idTransactionPost\": \"" # val.idTransactionPost # "\" , \"idProcess\": \"" # val.idProcess # "\" , \"idSubprocess\": \"" # val.idSubprocess # "\" , \"idHecta\": \"" # val.idHecta # "\" , \"idLote\": \"" # val.idLote # "\" , \"idProduct\": \"" # val.idProduct # "\" , \"gln\": \"" # val.gln # "\" , \"gtin\": \"" # val.gtin # "\" , \"register_date\": \"" # val.register_date # "\" , \"resp_id\": \"" # val.resp_id # "\" , \"weight\": \"" # val.weight # "\" , \"amount\": \"" # val.amount # "\"  ";
          }
        };
        resp := resp # "]}";
        {
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8(resp);
        streaming_strategy = null;
        upgrade = ?false;
      }};
      case ("POST", "/transaccion", 2) {{
        status_code = 200;
        headers = [ ("Content-Type","application/json;charset=utf-8") ];
        body = Text.encodeUtf8("{\n \"counter\" : "# Nat.toText(counter)# " }");
        streaming_strategy = null;
        upgrade = ?true;
      }};
      case _ {{
        status_code = 400;
        headers = [];
        body = "Invalid adffasrequest";
        streaming_strategy = null;
        upgrade = null;
      }};
    }
  };

  public func http_request_update(req : HttpRequest) : async HttpResponse {
    let requr = Iter.toArray(Text.split(req.url,#char '?'))[0];
    let resbo = Iter.toArray(Text.split(req.url,#char '?'))[1];
    switch (req.method,requr) {
        
      case ("POST", "/proceso") {

        var keyNew = processMap.size();
          for ((key,val)  in processMap.entries()){
          if (val.idProcess == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ){
            keyNew := key;
          }  ;
        };
        let process:Process = { 
          idProcess =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ;
          descProcess = Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1];
        };
        processMap.put(keyNew, process );
        {
          status_code = 201;
          headers = [ ("Content-Type","application/json;charset=utf-8")  ];
          body = "{\"res\": \"ok\", \"message\": \"se ha creado el proceso \" }";
          streaming_strategy = null;
          upgrade = null;
        }
      };
       case ("POST", "/subproceso") {

        var keyNew = subProcessMap.size();
          for ((key,val)  in subProcessMap
          .entries()){
          if (val.idSubProcess == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1] ){
            keyNew := key;
          }  ;
        };
        let subprocess:SubProcess = { 
          idProcess =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ;
          idSubProcess = Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1] ;
          descSubProcess = Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[2],#char '='))[1];
        };
        subProcessMap.put(keyNew, subprocess );
        {
          status_code = 201;
          headers = [ ("Content-Type","application/json;charset=utf-8")  ];
          body = "{\"res\": \"ok\", \"message\": \"se ha creado el subproceso \" }";
          streaming_strategy = null;
          upgrade = null;
        }
      };
       case ("POST", "/hectarea") {

        var keyNew = hectaMap.size();
          for ((key,val)  in hectaMap.entries()){
          if (val.idHectarea == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ){
            keyNew := key;
          }  ;
        };
        let hectarea:Hecta = { 
          idHectarea =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ;
          descHecta = Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1];
        };
        hectaMap.put(keyNew, hectarea );
        {
          status_code = 201;
          headers = [ ("Content-Type","application/json;charset=utf-8")  ];
          body = "{\"res\": \"ok\", \"message\": \"se ha creado la hectarea \" }";
          streaming_strategy = null;
          upgrade = null;
        }
      };
       case ("POST", "/lote") {

        var keyNew = lotesMap.size();
          for ((key,val)  in lotesMap.entries()){
          if (val.idLote == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ){
            keyNew := key;
          }  ;
        };
        let lote:Lote = { 
          idLote =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ;
          descLote = Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1];
        };
        lotesMap.put(keyNew, lote );
        {
          status_code = 201;
          headers = [ ("Content-Type","application/json;charset=utf-8")  ];
          body = "{\"res\": \"ok\", \"message\": \"se ha creado el lote \" }";
          streaming_strategy = null;
          upgrade = null;
        }
      };
       case ("POST", "/producto") {

        var keyNew = productMap.size();
          for ((key,val)  in productMap.entries()){
          if (val.idProduct == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ){
            keyNew := key;
          }  ;
        };
        let producto:Product = { 
          idProduct =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ;
          descProduct = Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1];
        };
        productMap.put(keyNew, producto );
        {
          status_code = 201;
          headers = [ ("Content-Type","application/json;charset=utf-8")  ];
          body = "{\"res\": \"ok\", \"message\": \"se ha creado el producto \" }";
          streaming_strategy = null;
          upgrade = null;
        }
      };
       case ("POST", "/transaccion") {

        var keyNew = transactionsMap.size();
          for ((key,val)  in transactionsMap.entries()){
          if (val.idTransaction == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ){
            keyNew := key;
          }  ;
        };
        let transaction:Transaction = {
           idTransaction =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1];
            idTransactionPrev =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[1],#char '='))[1];
            idTransactionPost =   "";
            idProcess =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[2],#char '='))[1];
            idSubprocess =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[3],#char '='))[1];
          idHecta =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[4],#char '='))[1];
          idLote =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[5],#char '='))[1];
    idProduct =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[6],#char '='))[1];
    gln =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[7],#char '='))[1];
    gtin =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[8],#char '='))[1];
    register_date =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[9],#char '='))[1];
    resp_id =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[10],#char '='))[1];
    weight =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[11],#char '='))[1];
    amount =   Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[12],#char '='))[1];
    hash_id_transaction = Blob.hash(Text.encodeUtf8(Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1]));
        };

        var tran : Transaction = transaction;
        //update transactionpast
          var keyNew_1 = transactionsMap.size();
          for ((key,val)  in transactionsMap.entries()){
          if (val.idTransaction == Iter.toArray(Text.split(Iter.toArray(Text.split(resbo,#char '&' ))[0],#char '='))[1] ){
            keyNew_1 := key;
            tran := val;
          }  ;
        };
        let newTran: Transaction = {
          idTransaction =   tran.idTransaction;
          idTransactionPrev =   tran.idTransactionPrev;
          idTransactionPost =   transaction.idTransaction;
          idProcess =   tran.idProcess;
          idSubprocess =  tran.idSubprocess;
          idHecta =   tran.idHecta;
          idLote =   tran.idLote;
          idProduct =   tran.idProduct;
          gln =   tran.gln;
          gtin =   tran.gtin;
          register_date =   tran.register_date;
          resp_id =   tran.resp_id;
          weight =   tran.weight;
          amount =   tran.amount;
          hash_id_transaction = tran.hash_id_transaction;
        };
        transactionsMap.put(keyNew, transaction );
        transactionsMap.put(keyNew_1,newTran);
        {
          status_code = 201;
          headers = [ ("Content-Type","application/json;charset=utf-8")  ];
          body = Text.encodeUtf8("{\"res\": \"ok\", \"message\": \"se ha creado la transaccion \", \"hash\" : \"" # Nat32.toText(transaction.hash_id_transaction) # "\" }");
          streaming_strategy = null;
          upgrade = null;
        }
      };
      
      case _ {{
        status_code = 400;
        headers = [];
        body = "Invalid asas request";
        streaming_strategy = null;
        upgrade = null;
      }};
    }
  };

  system func preupgrade(){
    processes := Iter.toArray(processMap.entries());
    subProcesses := Iter.toArray(subProcessMap.entries());
    hectas := Iter.toArray(hectaMap.entries());
    lotes := Iter.toArray(lotesMap.entries());
    products := Iter.toArray(productMap.entries());
    transactions := Iter.toArray(transactionsMap.entries());
    transactions := [];
  }
};