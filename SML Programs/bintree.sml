datatype 'a bintree =
        Empty |
        Node of 'a * 'a bintree * 'a bintree

exception empty_binary_tree

fun isEmpty Empty = true (* if empty is true then return 1 *)
    | isEmpty _ = false

fun subTrees Empty = raise empty_binary_tree
    | subTrees(Node(n, lst, rst)) = (lst, rst)

fun root Empty = raise empty_binary_tree
    | root(Node(n, _, _)) = n

fun leftSubTree Empty = raise empty_binary_tree
    | leftSubTree(Node(_, lst, _)) = lst

fun rightSubTree Empty = raise empty_binary_tree
    | rightSubTree(Node(_, _, rst)) = rst

fun height Empty = 0
    | height(Node(n, lst, rst)) =
    1 + Int.max(height(lst), height(rst))

fun isBalanced Empty = true
    | isBalanced(Node(n, lst, rst)) =
    (abs(height(lst) - height(rst)) <= 1) andalso
    isBalanced(lst) andalso isBalanced(rst)

fun size Empty = 0
    | size(Node(n, lst, rst)) =
    1 + size(lst) + size(rst)

fun preOrder1 Empty = nil
    | preOrder1 (Node(n, lst, rst)) =
            [n] @ preOrder1(lst) @ preOrder1(rst)

 (* efficient implementation of preOrder traversal *)
 local fun pre(Empty, Llist) = Llist
        | pre(Node(n, lst, rst), Llist) =
            let val Mlist = pre(rst, Llist)
                val Nlist = pre(lst, Mlist)
            in n::Nlist
            end
in fun preOrder2 T = pre(T, [])
end

(* preOrder1 Node((1,(2,(4, Empty, Empty), (5, Empty, Empty)), (3, (6, Empty, Empty), (7, Empty, Empty)))); *)
