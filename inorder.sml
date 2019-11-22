

signature BINTREE =
sig
datatype 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree

val inorder  : 'a bintree -> ('a option * int * int) list

  
val getree  : ('a option * 'b * 'c) bintree -> 'a bintree

val getvalue  : ('a option * 'b * 'c) bintree -> 'a option * 'b * 'c
val transform : 'a option * 'b * 'c -> ('a option * 'b * 'c) bintree

val combine  : (''a option * int * int) bintree list 
-> (''a option * int * int) bintree list
exception InvalidInorder
val inorderinverse1  : (''a option * int * int) bintree list -> ''a bintree
val inorderInverse : (''a option * int * int) list -> ''a bintree
end


structure Bintree : BINTREE =
struct
datatype 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree

local fun ino (Empty, Llist,l,a) = ((NONE),l,a)::Llist
| ino (Node (N, LST, RST), Llist,l,a) =
let val Mlist = ino (RST, Llist,l+1,1)
val Nlist = ino (LST, ((SOME N),l,a)::Mlist,l+1,0)
in Nlist
end
in fun inorder T = ino (T, [],1,2)
end;

val k=Node(1,Node(2,Node(4,Empty,Empty),Node(6,Empty,Empty)),Node(3,
Node(7,Empty,Empty),Node(5,Empty,Empty)));
(* It will return proper inorder traversal *)

val k=Empty;
(* It will return NONE value *)

val k=Node(1,Node(2,Empty,Node(6,Empty,Empty)),Node(3,Node
(7,Empty,Empty),Empty));(* It will return proper inorder traversal *)

val k=Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(7,Empty,Empty)
,Empty));(* It will return proper inorder traversal *)

val k=Node(1,Node(2,Empty,Node(4,Empty,Empty)),Node(3,Node(5,Empty,Empty),
Node(6,Node(7,Empty,Empty),Empty)));
(* It will return proper inorder traversal *)


fun getree (Node((NONE,x,y),Empty,Empty))= Empty
| getree (Node((SOME l,x,y),lst,rst))=
let 
val left=getree lst
val right=getree rst
in (Node(l,left,right))
end;


fun getvalue (Node((NONE,x,y),_,_))=(NONE,x,y)
| getvalue (Node((SOME K,x,y),_,_))=(SOME K,x,y)

fun transform (NONE,l,a)=(Node((NONE,l,a),Empty,Empty))
| transform ((SOME x),l,a)=(Node(((SOME x),l,a),Empty,Empty));


exception InvalidInorder;

fun combine []=[]
| combine [l]=[l]
| combine [l,m]=[l,m]
| combine (b1::b2::b3::b)=
let 
val (x1,y1,z1)=getvalue b1
val (x2,y2,z2)=getvalue b2
val (x3,y3,z3)=getvalue b3
in if b=[] andalso y1=y3 andalso y1=y2+1 andalso z1=0 andalso z3=1 
andalso x1=NONE andalso x3=NONE andalso z2<>2  then raise InvalidInorder
else if y1=y3 andalso y1=y2+1 andalso z1=0 andalso z3=1 andalso x1=NONE
 andalso x2=NONE andalso x3=NONE then raise InvalidInorder
else if y1=y3 andalso y1=y2+1 andalso z1=0 andalso z3=1 then 
(Node((x2,y2,z2),b1,b3))::(combine b)
else b1::(combine (b2::(b3::b)))
end


exception InvalidInorder;

fun inorderinverse1 []=raise InvalidInorder
| inorderinverse1 [_,_]=raise InvalidInorder
| inorderinverse1 [l1]= getree l1
| inorderinverse1 L=
let 
val M=combine L
in if M=L then raise InvalidInorder else inorderinverse1 M
end;


exception InvalidInorder;

fun inorderInverse []=raise InvalidInorder
| inorderInverse [(NONE,2,1)]=Empty
| inorderInverse [(NONE,_,_)]=raise InvalidInorder
| inorderInverse L= inorderinverse1 (map transform L)



val k=[(NONE,2,0),(SOME 1,1,2),(NONE,2,1),(NONE,2,1)]; 
(* raise InvalidInorder exception because list will convert into list of 
two elements*)

val k=[(NONE,1,2)];
(* It will return Empty *)
  
val k=[(NONE,2,0),(SOME 1,1,2),(NONE,2,0)];
(* raise InvalidInorder exception because two children have same direction *)          

val k=[(NONE,2,0),(NONE,1,2),(NONE,2,0)];   
(* raise InvalidInorder exception because three consecutive nodes are 
of NONE type *)          
    

val k=[(NONE,2,0),(SOME 1,1,1),(NONE,2,1)];
(* raise InvalidInorder exception because no node is parent node *)

val k=[(NONE,2,0),(SOME 1,1,2),(SOME 1,1,2),(NONE,1,2),(NONE,2,1)]; 
(* raise InvalidInorder exception because no node is parent node *)

val k=[(NONE,4,0),(SOME 4,3,0),(NONE,4,1),(SOME 2,2,0),(NONE,3,1),
(SOME 1,1,2),(NONE,4,0),(SOME 7,3,0),(NONE,4,1),(SOME 3,2,1),
(NONE,3,1)];(* generate valid tree *)

val k=[(NONE,3,0),(SOME 2,2,0),(NONE,4,0),(SOME 6,3,1),(NONE,4,1)
,(SOME 1,1,2),(NONE,4,0),(SOME 7,3,0),(NONE,4,1),(SOME 3,2,1),(NONE,3,1)];
(* generate valid tree *)


end

 open Bintree;
