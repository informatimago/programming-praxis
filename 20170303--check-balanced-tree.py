# assume: tree is None or left(tree), right(tree) are trees                     
 
def dep(tree):
    '''Return the depth of the tree if it's balanced, None if it's not.         
                                                                                 
    '''
    if tree is None: return 0
    west = dep(left(tree))
    if west is None: return None
    east = dep(right(tree))
    if east is None: return None
    if -1 <= west - east <= 1: return 1 + max(west, east)
    return None
 
def isbalanced(tree): return dep(tree) is not None



def makeNode(left=None, right=None):
    (left, right)


def left(node):
    node[0]


def right(node):
    node[1]
    
left = makeNode(left=makeNode(left=makeNode(left=makeNode(left=makeNode()))))
print("# {} is {}balanced.".format('left',"" if isbalanced(left) else "not "))

# left is balanced.
