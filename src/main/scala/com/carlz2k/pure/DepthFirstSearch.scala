package com.carlz2k.pure

/**
  * <i>Copyright (c) 2017, Payfirma Corporation. All rights reserved.</i>
  * <p>
  * carl.zhang add class description
  */

class DepthFirstSearch[T] {

  type Vertex = T
  type GraphMap = Map[Vertex,List[Vertex]]

  def BFS(start: Vertex, g: GraphMap): List[List[Vertex]] = {

    def BFS0(elems: List[Vertex],visited: List[List[Vertex]]): List[List[Vertex]] = {
      val newNeighbors = elems.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        visited
      else
        BFS0(newNeighbors, newNeighbors :: visited)
    }

    BFS0(List(start),List(List(start))).reverse
  }

  def DFS(start: Vertex, g: GraphMap): List[Vertex] = {

    def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v))
        visited
      else {
        val neighbours:List[Vertex] = g(v) filterNot visited.contains
        neighbours.foldLeft(v :: visited)((b,a) => DFS0(a,b))
      }
    }
    DFS0(start,List()).reverse
  }

  def longestUniqueSubsttr(str: String): Int = {
    val n = str.length
    var cur_len = 1
    // length of current substring
    var max_len = 1
    // result
    var prev_index = 0
    //  previous index
    var i = 0
    val visited = new Map()
    /* Initialize the visited array as -1, -1 is
             used to indicate that character has not been
             visited yet. */ i = 0
    while ( {
      i < NO_OF_CHARS
    }) {
      visited(i) = -1

      {
        i += 1; i - 1
      }
    }
    /* Mark first character as visited by storing the
                 index of first   character in visited array. */ visited(str.charAt(0)) = 0
    /* Start from the second character. First character is
               already processed (cur_len and max_len are initialized
             as 1, and visited[str[0]] is set */ i = 1
    while ( {
      i < n
    }) {
      prev_index = visited(str.charAt(i))
      /* If the current character is not present in
                 the already processed substring or it is not
                    part of the current NRCS, then do cur_len++ */ if (prev_index == -1 || i - cur_len > prev_index) {
        cur_len += 1; cur_len - 1
      }
      else {
        /* If the current character is present in currently
                      considered NRCS, then update NRCS to start from
                      the next character of previous instance. *//* Also, when we are changing the NRCS, we
                          should also check whether length of the
                          previous NRCS was greater than max_len or
                          not.*/ if (cur_len > max_len) max_len = cur_len
        cur_len = i - prev_index
      }
      // update the index of current character
      visited(str.charAt(i)) = i

      {
        i += 1; i - 1
      }
    }
    // Compare the length of last NRCS with max_len and
    // update max_len if needed
    if (cur_len > max_len) max_len = cur_len
    max_len
  }
}
