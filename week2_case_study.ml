(* A phone number is a sequence of four integers. *)
type phone_number = int * int * int * int;;

(* A contact has a name and a phone number. *)
type contact = {
  name         : string;
  phone_number : phone_number
};;

(* Here is a dumb contact. *)
let nobody = { name = ""; phone_number = (0, 0, 0, 0) };;

(* A database is a collection of contacts. *)
type database = {
  number_of_contacts : int;
  contacts : contact array;
};;

(* [make n] is the database with no contact and at most [n] contacts
    stored inside. *)
let make max_number_of_contacts =
  {
    number_of_contacts = 0;
    contacts = Array.make max_number_of_contacts nobody
  };;

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
  code    : int;
  contact : contact;
}

let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
  in
  aux 0;;

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
      (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
	      if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
          number_of_contacts = db.number_of_contacts + 1;
          contacts = Array.init (Array.length db.contacts) cells
        }
      in
      (true, db', contact);;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        nobody
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;

(* Engine parses and interprets the query. *)
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody);;


(* Assignment *)
let db_ = make 5;;
let rec apply db queries =
  let length = Array.length queries in
    if length == 0 then db else
    let _, db', _ = engine db queries.(0) in apply db' (Array.sub queries 1 (length - 1));;

let proof_of_bug =
  [|
    { code=0; contact={ name="me"; phone_number=(1,1,1,1) } };
    { code=0; contact={ name="me1"; phone_number=(1,1,1,1) } };
    { code=0; contact={ name="me2"; phone_number=(1,1,1,1) } };
    { code=1; contact={ name="me1"; phone_number=(1,1,1,1) } };
    (* { code=0; contact={ name="me3"; phone_number=(1,1,1,1) } }; *)
    (* { code=1; contact={ name="me2"; phone_number=(1,1,1,1) } }; *)
  |] ;;

let deleted = { name = "deleted"; phone_number = (0, 0, 0, 0) };;

let compress db =
  (* loop thru until we find nobody, replace that with where contact should be  *)
  let rec aux db i =
    if db.contacts.(i) != deleted then aux db (i + 1) else
    let cells i =
      if db.contacts.(i) = deleted then
        db.contacts.(db.number_of_contacts)
      else if i >= db.number_of_contacts then
        nobody
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in db'
  in aux db 0;;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        deleted
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, compress db', contact);;


(* Write a new function update :
database -> contact -> (bool * database * contact) that either changes the
number of an existing person or inserts a new contact. It should return true
and the updated database if any of these two options succeeded, or false with
the untouched database. The returned contact is not important, it is here just
so the function has the same signature as the others. *)
let update db contact =
  (* search db contact *)
  let contact_exists, _, _ = search db contact in
    if not contact_exists then insert db contact else
    let cells i =
      if db.contacts.(i).name = contact.name then
        { name = contact.name; phone_number = contact.phone_number }
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in (true, db', contact);;

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
