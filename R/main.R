#' Generates the solution when only singletons are yield
#'
#' @param total_nodos Total number of nodes in data matrix.
#' @param nodos_singletons Nodes list with cluster singletons.
#' @return \code{clusteres_unidos} An object of class "igraph" as a network representing the clustering solution.
#
#
#' @keywords internal
#' @export
only.single.graphs <- function(total_nodos, nodos_singletons) {

  #Only there are singletons in subgraph hence clusters will be formed only with them
  clusteres_unidos = igraph::graph(edges =,NULL,n=NULL,directed =FALSE)
  clusteres_unidos = clusteres_unidos + igraph::vertices(total_nodos)

  if(length(nodos_singletons)>0) {
    single=as.character(unlist(unique(nodos_singletons)))
  }

  if(exists("single")){
    clusteres_unidos = clusteres_unidos + igraph::path(single)
  }



  return(clusteres_unidos)

}





#' Generates a complete graph
#'
#' @description This function generates a complete graph. Where each node represents a object_i and the edges have a cost representing the distance d_ij between object_i and other object_j.
#' @param nodes.list A vector with a subset of nodes (objects) of the data matrix for which the complete graph must be generated.
#' @param distance.matrix A distance matrix between each pair of objects in \code{nodes.list}. It is used as the edges costs to generate the complete graph.
#' @return \item{edges.complete.graph}{A object of class "data.frame" with three columns (\emph{object_i}, \emph{object_j}, \emph{d_ij}) representing the distance between object \emph{i} and object \emph{j} of the distance matrix. For instance:}
#' \tabular{llc}{
#'  \emph{object_i} \tab  \emph{object_j} \tab  \emph{d_ij}\cr
#'  1 \tab 2 \tab 1.60\cr
#'  1 \tab 3 \tab 0.08\cr
#'  1 \tab 4 \tab 1.21\cr
#'  ... \tab ... \tab ...\cr
#'  n-1 \tab n \tab ...\cr
#' }
#' @author  Mario Inostroza-Ponta, Jorge Parraga-Alava, Pablo Moscato
#' @examples
#'
#' set.seed(1987)
#'
#' ##Generates a data matrix of dimension 50X13
#'
#' n=50; m=13
#' x <- matrix(runif(n*m, min = -5, max = 10), nrow=n, ncol=m)
#'
#' ##Computes a distance matrix of x.
#'
#' library("stats")
#' d <- base::as.matrix(stats::dist(x, method="euclidean"))
#'
#' ##Generates complete graph (CG)
#'
#' cg <- generate.complete.graph(1:nrow(x),d)
#'
#' head(cg)
#'
#' ##Visualizing CG graph
#' library("igraph")
#' cg.network=igraph::graph.adjacency(d, mode="undirected", weighted=TRUE)
#' plot(cg.network, edge.label=round(E(cg.network)$weight, 2), main="Complete Graph")
#'
#' @keywords internal
#' @export
generate.complete.graph <-function(nodes.list, distance.matrix){

  aristas= t(utils::combn(nodes.list,2))
  colnames(aristas)=c("object_i","object_j")

  d_ij=distance.matrix[aristas]

  aristas=cbind(aristas,d_ij)


  return(edges.complete.graph=as.data.frame(aristas))

}

 

#' Generates a kNN graph
#'
#' @description This function generates the \emph{k}-Nearest Neighbors (kNN) graph which is a subgraph contains edges between nodes if, and only if, they are one of the \emph{k} nearest neighbors considering the edges costs (distances). Each node represents an object of the complete graph.
#' @param edges.complete.graph A object of class "data.frame" with three columns (\emph{object_i}, \emph{object_j}, \emph{d_ij}) representing the distance \emph{d_ij} between \emph{object_i} and \emph{object_j}.
#' @param suggested.k  It is an optional argument. A numeric value representing the suggested number of \emph{k}-nearest neighbors to consider to generate the \emph{k}NN graph.
#' @details During its generation, the \emph{k} value is automatically determined by the definition:
#' \deqn{k = min{\lfloor \ln(|nodes.list|) \rfloor; min k |  kNN  is connected; suggested.k }}
#' If \emph{suggested.k} parameter is not provided, it is not considered by the definition.
#' @return A list with the elements
#' \item{edges.knn.graph}{A object of class "data.frame" with three columns (\emph{object_i}, \emph{object_j}, \emph{d_ij}) representing the \emph{d_ij} between \emph{object_i} and \emph{object_j} that are part of the \emph{k}NN graph.}
#' \item{knn.graph}{A object of class "igraph" which is the \emph{k}-Nearest Neighbors (kNN) graph generated.}
#' \item{k}{The \emph{k} value determined by the definition.}
#' @author  Mario Inostroza-Ponta, Jorge Parraga-Alava, Pablo Moscato
#' @examples
#'
#' set.seed(1987)
#'
#' ##Generates a data matrix of dimension 50X13
#' n=50; m=13
#' x <- matrix(runif(n*m, min = -5, max = 10), nrow=n, ncol=m)
#'
#' ##Computes a distance matrix of x.
#'
#' library("stats")
#' d <- base::as.matrix(stats::dist(x, method="euclidean"))
#'
#' ##Generates complete graph (CG) without suggested.k parameter
#'
#' cg <- generate.complete.graph(1:nrow(x),d)
#'
#' ##Generates kNN graph
#' knn <- generate.knn(cg)
#'
#' ##Visualizing kNN graph
#' plot(knn$knn.graph,
#' main=paste("kNN \n k=", knn$k, sep=""))
#'
#'
#'
#' ##Generates complete graph (CG) with suggested.k parameter
#'
#' cg <- generate.complete.graph(1:nrow(x),d)
#'
#' ##Generates kNN graph
#' knn <- generate.knn(cg, suggested.k=4)
#'
#' ##Visualizing kNN graph
#' plot(knn$knn.graph,
#' main=paste("kNN \n k=", knn$k, sep=""))
#' @keywords graph
#' @keywords knn
#' @export
generate.knn <- function(edges.complete.graph, suggested.k) {

  grafo_knn=list()
  arista_vecinos_unidas=list()
  grafo_knn_conectado=vector()
 
  #almacenada la lista de nodos
  nodos.object_i<-unique(edges.complete.graph$object_i)
  nodos.object_j<-unique(edges.complete.graph$object_j)
  nodos<-c(nodos.object_i, nodos.object_j)
  nodos<-unique(nodos)
  n <-length(nodos)
  #evaluo cada k desde 1 hasta n-1 para ver cual minimo k mantiene conectado al knn


          k=1

          while(k <=(n-1)){

            #ordeno tabla de pares de aristas de acuerdo al costo, y convierto en factor cada nodo
            aristas.ordenadas <- edges.complete.graph[order(edges.complete.graph$d_ij),]
            aristas.ordenadas$object_i <- as.factor(aristas.ordenadas$object_i)
            aristas.ordenadas$object_j <- as.factor(aristas.ordenadas$object_j)
            #separo la tabla de pares de aristas por cada nodo. Como estan ordenadas por costos, selecciono los k nodos mas cercanos al nodo
            #esto se aplica para nodos en from y luego para nodos en to
            vecinos.nodos.object_i<-do.call(rbind, lapply(split(aristas.ordenadas,aristas.ordenadas$object_i), function(x) {return(x[1:k,])}))
            vecinos.nodos.object_i<-stats::na.omit(vecinos.nodos.object_i)
            vecinos.nodos.object_j<-do.call(rbind, lapply(split(aristas.ordenadas,aristas.ordenadas$object_j), function(x) {return(x[1:k,])}))
            vecinos.nodos.object_j<-stats::na.omit(vecinos.nodos.object_j)
            #ubicar como primer columna el to, luego cambiar nombre a columnas para que
            #tenga mismos encabezados que vecinos.nodos.object_i y hacer poder usar el split
            vecinos.nodos.object_j<-vecinos.nodos.object_j[,c(2,1,3)]
            colnames(vecinos.nodos.object_j)<-c("object_i", "object_j", "d_ij")
            #uno ambos resultados (from y to)
            vecinos.nodos.ambos<-rbind(vecinos.nodos.object_i, vecinos.nodos.object_j)
            #vuelvo a ordenar el par de aristas, pero ahora estan solo los mas cercanos desde from y desde to.
            ambos.ordenados=vecinos.nodos.ambos[order(vecinos.nodos.ambos$d_ij),]
            #separo la tabla para seleccionar solo los k mas cercanos a cada nodo
            vecinos.final<-do.call(rbind, lapply(split(ambos.ordenados, ambos.ordenados$object_i), function(x) {return(x[1:k,])}))
            vecinos.final<-stats::na.omit(vecinos.final)


            #Eliminar filas repetidas tanto en from, to, cost
            arista_vecinos_unidas[[k]]=vecinos.final[!duplicated(vecinos.final), ]


                  #creo el knn. Para cada caso de k.
                  grafo_knn[[k]]=igraph::graph.data.frame(d = arista_vecinos_unidas[[k]][,1:2], directed = FALSE)
                  #eliminar aristas tipo lazos (o sea 1-3 y 3-1 dejar solo 1)
                  grafo_knn[[k]]=igraph::simplify(grafo_knn[[k]], remove.loops = TRUE, remove.multiple = FALSE)

                  #determino si este k, produce un knn conectado
                  grafo_knn_conectado[k]=igraph::is.connected(grafo_knn[[k]])

                  #Si este k esta conectado entonces ya no evaluo mas los k ya q es el minimo k que conecta
                  if(grafo_knn_conectado[k]==TRUE){
                    k=n
                  }else{
                    k=k+1
                  }


        }

          #minimo k que mantiene conectado el knn

          #evaluacion_k es vector booleano de cada k evaluado. True si el valor correspondiente k mantiene el knn conectado
          evaluacion_k=which(grafo_knn_conectado==TRUE)

          if(length(evaluacion_k)>0){
            #si evaluacion tiene largo >0 es porque algun k evaluado mantiene conectado el grafo. Por tanto de esos k escogo el menor
            k_conectado=min(evaluacion_k)
          }else{
            cat("\n In any k the graph knn can be connected. It will use as k the log(n). \n")
            k_conectado=n
          }

          #minimo Logaritmo natural.

          #usa funcion piso (floor): devuelve el maximo entero no superior a n
          k_natural=floor(log(n))




          if(missing(suggested.k)) {

            #Determinar min K para que knn esté conectado
            valor_k = min(k_natural, k_conectado)


          }else{
            k_user=k

            #Determinar min K para que knn esté conectado
            valor_k = min(k_natural, k_conectado, k_user)

          }



          #para casos que minimo valor_k es 0 entonces asigno como valor_k=1
          if(valor_k==0) {valor_k=1}






  #edges.knn.graph=igraph::get.edgelist(grafo_knn[[valor_k]], names=TRUE)

  return(list(edges.knn.graph=arista_vecinos_unidas[[valor_k]], k=valor_k, knn.graph=grafo_knn[[valor_k]]))




}






#' Generates a MST graph
#'
#' @description This function generates the Minimal Spanning Tree (MST) graph which is a connected and acyclic subgraph contains all the nodes of the complete graph (CG) and whose edges sum (distances) has minimum costs. Each node represents an object of the complete graph.
#' @param edges.complete.graph A object of class "data.frame" with three columns (\emph{object_i}, \emph{object_j}, \emph{d_ij}) representing the distance \emph{d_ij} between \emph{object i} and \emph{object j} of the complete graph.
#' @details Generation of MST graph is performed using the Prim's algorithm.
#' @return A list with the elements
#' \item{edges.mst.graph}{A object of class "data.frame" with three columns (\emph{object_i}, \emph{object_j}, \emph{d_ij}) representing the distance \emph{d_ij} between object \emph{i} and object \emph{j} that are part of the MST graph.}
#' \item{mst.graph}{A object of class "igraph" which is the Minimal Spanning Tree (MST) graph generated.}
#' @references
#' Prim, R.C. (1957). \emph{Shortest connection networks and some generalizations}. Bell System Technical Journal, 37 1389-1401.
#'
#' Ignatenkov, E. (2015). \emph{Minimum Spanning Tree (MST) for some graph using Prim's MST algorithm}. Stanford University course on Coursera.
#' @author  Mario Inostroza-Ponta, Jorge Parraga-Alava, Pablo Moscato
#' @examples
#'
#' set.seed(1987)
#'
#' ##Generates a data matrix of dimension 50X13
#' n=50; m=13
#' x <- matrix(runif(n*m, min = -5, max = 10), nrow=n, ncol=m)
#'
#' ##Computes a distance matrix of x.
#'
#' library("stats")
#' d <- base::as.matrix(stats::dist(x, method="euclidean"))
#'
#' ##Generates complete graph (CG)
#'
#' cg <- generate.complete.graph(1:nrow(x),d)
#'
#' ##Generates MST graph
#'
#' mstree <- generate.mst(cg)
#'
#' ##Visualizing MST graph
#' plot(mstree$mst.graph, main="MST")
#'
#'
#' @keywords graph
#' @keywords mst
#' @export
generate.mst <- function(edges.complete.graph) {

        #n=length(unique(unname(unlist(edges.complete.graph[,1:2])))) #cuenta el numero total de nodos
        #nodos= unique(unname(unlist(edges.complete.graph[,1:2]))) #almacenada la lista de nodos

        #almacenada la lista de nodos
        nodos.object_i<-unique(edges.complete.graph$object_i)
        nodos.object_j<-unique(edges.complete.graph$object_j)
        nodos<-c(nodos.object_i, nodos.object_j)
        nodos<-unique(nodos)
        n <-length(nodos)


        #A partir de aristas grafo completo genero un objeto igraph que
        #construye el mst con prim
        #completo=as.data.frame(edges.complete.graph)
        #creo objeto igraph de grafo completo
        gc.i=igraph::graph.data.frame(edges.complete.graph, directed = FALSE)
        #asigno costos al grafo completo
        igraph::E(gc.i)$weight=edges.complete.graph$d_ij
        #Construyo mst con prim
        gmst.i <- igraph::mst(gc.i, algorithm="prim")
        #convierto en tabla de aristas el objeto igraph mst
        tabla=as.data.frame(igraph::as_edgelist(gmst.i))
        tabla<-stats::setNames(tabla, c("object_i","object_j"))
        tabla$object_i=as.numeric(tabla$object_i) #from
        tabla$object_j=as.numeric(tabla$object_j) #to
        tabla$d_ij=igraph::E(gmst.i)$d_ij#cost

        #asigno mst (objeto igraph y tabla aristas) a las variables a retornar

        return(list(edges.mst.graph=as.data.frame(tabla), mst.graph=gmst.i))


        #Prim
        #Adapted from Egor Ignatenkov https://rstudio-pubs-static.s3.amazonaws.com/68800_142ed05b1c0741c18da00431ba1bdbf5.html

       #  mst<-vector()#vector of MST vertices
       #  mstedges <-list() #MST edges
       #
       #  mst[1]=nodos[1]
       #
       #
       #  #It is MUCH faster to create the results an empty vector of the correct size, and modify elements in place
       #  #Modificacion visita a stgo nov 2018
       #  #cand<-numeric(length(nodos))
       # # nxt<-list()
       #  #mst<-numeric(length(nodos))
       #
       #  j=1 #controla la fila de las tabla de aristas mst
       #  for (i in nodos[-1]){
       #      #choosing only that edges that has one end in current MST and another end not in MST
       #      cand=graph[xor(graph$object_j %in% mst, graph$object_i %in% mst),]
       #      #order the candidate edges by its cost and choose the cheapest
       #      # nxt[[i]]=as.vector(graph[xor(graph$object_j %in% mst, graph$object_i %in% mst),][order(cand$d_ij)[1],])
       #      nxt=as.vector(cand[order(cand$d_ij)[1],])
       #      #add new vertices to MST and drop repeated vertices
       #      mst=unique(c(mst,as.numeric(nxt[1]),as.numeric(nxt[2])))
       #      #save new edge into mstedge
       #      mstedges[[j]]=c(from=as.numeric(nxt[1]), to=as.numeric(nxt[2]), costs=as.numeric(nxt[[i]][3]))
       #      j=j+1
       #  }
       #
       #  aristas_mst=do.call("rbind", mstedges)
       #  aristas_mst=as.data.frame(aristas_mst)
       #  mst.graph=igraph::graph.data.frame(aristas_mst, directed = FALSE)
       #
       #
       #
       #  #return(mst.graph)
       #  return(list(edges.mst.graph=aristas_mst, mst.graph=mst.graph))



}








#' Computes the edge costs sum of a proximity graph
#'
#' @description This function computes the edge costs (distances) overall sum of a proximity graph.
#' @param graph.edges A object of class "matrix" with two columns (\emph{object_i}, \emph{object_j}) representing the objects (nodes) of a proximity graph.
#' @param distance.matrix A distance matrix between each pair of object i,j in the proximity graph.
#' @return \item{total.costs.graph}{A numeric value representing the edge costs (distance) overall sum of a proximity graph.}
#' @examples
#'
#' set.seed(1987)
#'
#' ##Generates a data matrix of dimension 50X13
#' n=50; m=13
#' x <- matrix(runif(n*m, min = -5, max = 10), nrow=n, ncol=m)
#'
#' ##Computes a distance matrix of x.
#'
#' library("stats")
#' d <- base::as.matrix(stats::dist(x, method="euclidean"))
#'
#' ##Generates complete graph (CG)
#'
#' cg <- generate.complete.graph(1:nrow(x),d)
#'
#' ##Generates a proximity graph (MST)
#' mstree <- generate.mst(cg)
#'
#' ##Calculate the edge cost sum of proximity graph (MST)
#' mstree.cost=as.numeric(compute.costs.proximity.graph(as.matrix(mstree$edges.mst.graph[,1:2]), d))
#' mstree.cost
#'
#' ##Generates a proximity graph (kNN)
#' knneig <- generate.knn(cg)
#'
#' ##Calculate the edge cost sum of proximity graph (kNN)
#' knneig.cost=as.numeric(compute.costs.proximity.graph(as.matrix(knneig$edges.knn.graph[,1:2]), d))
#' knneig.cost
#' @keywords internal
#' @export
compute.costs.proximity.graph=function(graph.edges, distance.matrix){

  class(graph.edges)<-"numeric"

  total_costs=sum(distance.matrix[graph.edges])

  total.costs.graph=as.matrix(t(c(costs=total_costs)))


  return(total.costs.graph)

}




#' Performs the intersections between MST y kNN graphs
#'
#' @description This function performs a graph partition based on the intersection of the edges of two proximity graphs: MST and kNN.
#' @param nodes.list A vector with a subset of objects (nodes) of the data matrix for which the MST y kNN graphs must be generated.
#' @param distance.matrix A distance matrix between each pair of elements in \code{nodes.list}. It is used as the edges costs to generate MST y kNN graphs.
#' @param suggested.k A numeric value representing the number of nearest neighbors to consider to generate the \emph{k}NN graph.
#' @return A list with the elements
#' \item{cc}{A numeric value representing the number of connected components (cc) generated after graphs intersection.}
#' \item{subgraphs}{ A list where each item contains the nodes of the connected components (cc) generated.}
#' \item{ccgraph}{A object of class "igraph" which is a network with each connected components (cc) generated.}
#' @author  Mario Inostroza-Ponta, Jorge Parraga-Alava, Pablo Moscato
#' @keywords internal
generate.intersections.mst.knn <- function(nodes.list, distance.matrix, suggested.k){


  #            Order nodes.list to  edges tin complete graphs be ordered.
  nodes.list=base::sort(nodes.list)

  if(length(nodes.list)>1){


            #            Generate complete graph (CG)               #

            edges.complete.graph=generate.complete.graph(nodes.list, distance.matrix)
            #grafo_completo_arbol=igraph::graph.data.frame(d = edges.complete.graph[,1:2], directed = FALSE)

            #            Generate MST graph                #
            ###mst.graph=generate.mst(edges.complete.graph)
            ###mst_graph_edges=igraph::get.edgelist(mst.graph, names = TRUE)
            ###grafo_mst_costo_total=compute.total.costs.mst(mst_graph_edges, distance.matrix)
            calcula_mst=generate.mst(edges.complete.graph)
            #grafo_mst_aristas=calcula_mst$edges.mst.graph
            ###grafo_mst_arbol=igraph::graph.edgelist(grafo_mst_aristas, directed = FALSE)
            grafo_mst_arbol=calcula_mst$mst.graph
            ###grafo_mst_costo_total=compute.total.costs.mst(as.matrix(grafo_mst_aristas[,1:2]), distance.matrix)

            #            Generate kNN graph                #
            calcula_knn=generate.knn(edges.complete.graph, suggested.k)
            #grafo_knn_aristas=calcula_knn$edges.knn.graph
            grafo_knn_arbol=calcula_knn$knn.graph
            ###grafo_knn_costo_total=compute.total.costs.knn(grafo_knn_aristas, distance.matrix)

            #            Perform MST, kNN graphs intersection                #
            grafo_interseccion_arbol <- igraph::graph.intersection(grafo_mst_arbol, grafo_knn_arbol, keep.all.vertices = TRUE)
            #grafo_interseccion_aristas <- igraph::get.edgelist(grafo_interseccion_arbol, names = TRUE)

            #            Determine components connected (CC)                #
            cc=igraph::components(grafo_interseccion_arbol)
            #separo todo los subgraphs resultantes luego de la interseccion
            sub_grafos=lapply(seq_along(cc$csize)[cc$csize > 0], function(x) igraph::V(grafo_interseccion_arbol)$name[cc$membership %in% x])
            #convierto los subgraphs como nodos tipo numero
            sub_grafos=lapply(sub_grafos, function (x) as.numeric(x))


            return(list(subgraphs=sub_grafos, ccgraph=grafo_interseccion_arbol, cc=cc$no))#, mst_costs=grafo_mst_costo_total, knnn_costs=grafo_knn_costo_total))


  }else{
    stop("\n. Error. Only there is one object (node).")
  }




}










#' Generates clustering results
#
#' @description This function performs the union the all component connected (cc) yield in each recursion of the MST-kNN algorithm.
#' @param g_clusters A object of class igraph containing all component connected (cc=1).
#' @param distance.matrix  A numeric matrix or data.frame with equals names and numbers of rows and columns representing objects to group.
#' @return A list with the elements
#' \item{cnumber}{A numeric value representing the number of clusters of the solution.}
#' \item{cluster}{A named vector of integers of size n with values in range \code{1:cnumber}, representing the cluster to which each object is assigned.}
#' \item{partition}{A partition matrix order by cluster where are shown the objects and the cluster where they are assigned.}
#' \item{csize}{A vector of size k with the cardinality of each cluster in the solution.}
#' \item{network}{An object of class "igraph" as a network representing the clustering solution.}
#' @keywords internal
#' @export
generate.results <-function(g_clusters, distance.matrix){

  #           Separate object igraph in clusters
  final_grupos=igraph::clusters(g_clusters)
  final_grupos_membresia=final_grupos$membership
  final_grupos_num_clusters=final_grupos$no
  final_grupos_cardinalidad=final_grupos$csize

  #           Convert object igraph (g_clusters) in object matrix of edges
  matriz_aristas_grupos=as.matrix(igraph::get.edgelist(g_clusters, names = TRUE))
  class(matriz_aristas_grupos)="integer"

  #           Get nodes list in clustering solution.
  nodos_en_solucion=sort(unique(c(matriz_aristas_grupos[,1], matriz_aristas_grupos[,2])))

  #           Create partition vector
  #vector_particion=final_grupos_membresia[sort(as.integer(names(final_grupos_membresia)), index.return = TRUE)$ix]
  vector_particion=unname(final_grupos_membresia[sort(as.integer(names(final_grupos_membresia)), index.return = TRUE)$ix])
  names(vector_particion)<-rownames(distance.matrix)


  #           Create partition matrix
  tabla_particion = data.frame(#object=rownames(distance.matrix)[nodos_en_solucion],
                               object=rownames(distance.matrix),
                               cluster=vector_particion, stringsAsFactors = FALSE)
  #           Order by number of cluster the partition matrix
  tabla_particion = tabla_particion[order(tabla_particion$cluster),]
  rownames(tabla_particion)=1:nrow(tabla_particion)

  #           Assigna names to objects (nodes) according to row namesdistance matrix
  elementos_orden=as.integer(names(final_grupos_membresia))
  #g_clusters=igraph::set.vertex.attribute(g_clusters, "name", value=rownames(distance.matrix)[elementos_orden])
  g_clusters=igraph::set.vertex.attribute(g_clusters, "name", value=elementos_orden)



return(list(network=g_clusters, cnumber=final_grupos_num_clusters, cluster= vector_particion,
               partition=tabla_particion, csize=final_grupos_cardinalidad))

}





















#' Performs the MST-kNN clustering algorithm
#'
#' @description Performs the MST-kNN clustering algorithm which generates a clustering solution with automatic \emph{number of clusters} determination using two proximity graphs: Minimal Spanning Tree (MST) and k-Nearest Neighbor (\emph{k}NN) which are recursively intersected.
#'
#' To create MST, \emph{Prim} algorithm is used. To create \emph{k}NN,  \code{distance.matrix} passed as input is considered.
#' @param distance.matrix  A numeric matrix or data.frame with equals numbers of rows and columns representing distances between objects to group.
#' @param suggested.k It is an optional argument. A numeric value representing the suggested number of k-nearest neighbors to consider during the generating the kNN graph. Note that, due to the algorithm operation, this number may be different during the algorithm execution.
#' @details
#' To see more details of how MST-kNN works refers to the \href{../doc/guide.html}{quick guide}.
#'
#' @return A list with the elements
#' \item{cnumber}{A numeric value representing the number of clusters of the solution.}
#' \item{cluster}{A named vector of integers from \code{1:cnumber} representing the cluster to which each object is assigned.}
#' \item{partition}{A partition matrix order by cluster where are shown the objects and the cluster where they are assigned.}
#' \item{csize}{A vector with the cardinality of each cluster in the solution.}
#' \item{network}{An object of class "igraph" as a network representing the clustering solution.}
#' @references Inostroza-Ponta, M. (2008). \emph{An Integrated and Scalable Approach Based on Combinatorial Optimization Techniques for the Analysis of Microarray Data}. Ph.D. thesis, School of Electrical Engineering and Computer Science. University of Newcastle.
#' @author  Mario Inostroza-Ponta, Jorge Parraga-Alava, Pablo Moscato
#' @examples
#'
#' set.seed(1987)
#'
#' ##load package
#' library("mstknnclust")
#'
#' ##Generates a data matrix of dimension 100X15
#'
#' n=100; m=15
#' 
#' x <- matrix(runif(n*m, min = -5, max = 10), nrow=n, ncol=m)
#'
#' ##Computes a distance matrix of x.
#'
#' library("stats")
#' d <- base::as.matrix(stats::dist(x, method="euclidean"))
#'
#' ##Performs MST-kNN clustering using euclidean distance.
#'
#' results <- mst.knn(d)
#'
#' ## Visualizes the clustering solution
#'
#' library("igraph")
#' plot(results$network, vertex.size=8,
#'      vertex.color=igraph::clusters(results$network)$membership,
#'      layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
#'      main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))
#'
#' @export
mst.knn <- function(distance.matrix, suggested.k){

  #      Validation of inputs     #


  if(!missing(distance.matrix)) {


    if((base::nrow(distance.matrix)==base::ncol(distance.matrix))){


      #if(base::identical(rownames(distance.matrix), rownames(distance.matrix))){

          distance.matrix=as.data.frame(distance.matrix)

          ############################# START

                  subgraphs=1:nrow(distance.matrix)

                  #            Initialization               #
                  inicio=generate.intersections.mst.knn(subgraphs, distance.matrix, suggested.k)

                  #          Recursivity on each CC           #
                  nodos_singletons=list()
                  nodos_singletons=c(nodos_singletons,Filter(function(x) length(x)<=1, inicio$subgraphs) )

                  #           Update subgraphs list using those have two o more nodes #
                  subgraphs=Filter(function(x) length(x)>=2, inicio$subgraphs)

                  #           Create list where are stored subgraphs with cc=1 (o sea el que es cluster). List store object igraph
                  cluster_subgraphs=list() ; x=1

                  #           Update subgraphs list (those have cc==1)
                  if(inicio$cc==1){
                    #Guardo como cluster solo si el cc=1 tiene 2 o mas nodos, o sea no guardo ningun singletons.
                    if(length(igraph::V(inicio$ccgraph))>1){
                      cluster_subgraphs[[x]]=inicio$ccgraph
                    }
                  }



                  # ----- Re-run mst.knn on each subgraph with cc>1 ------ #


                  #           Initialize cc as number of objects (nrow(distance.matrix))
                  cc=nrow(distance.matrix)


                  while(cc>1){

                            #           Create a un list of subgraphs CC>1 will be re-used in mst.knn
                            acumulados=list()


                            if(length(subgraphs)>0){

                                  for(y in 1:length(subgraphs)){

                                    #           Perform intersections on each subgraph with cc>1
                                    componente=generate.intersections.mst.knn(subgraphs[[y]], distance.matrix, suggested.k)

                                    #           Store possible singletons nodes obtained after re-run mst.knn
                                    nodos_singletons=c(nodos_singletons, Filter(function(x) length(x)<=1, componente$subgraphs))

                                    #           Update the list of with cc=1
                                    if(componente$cc==1){
                                          # If it has two o more nodes then stored. (It prevents to store singletons node)
                                          if(length(igraph::V(componente$ccgraph))>1){
                                            cluster_subgraphs[[x]]=componente$ccgraph
                                            x=x+1
                                          }
                                    }else{

                                          #           Store all subgraphs with two or more cc
                                          nodos_tres_mas=Filter(function(x) length(x)>=2, componente$subgraphs)

                                          if(length(nodos_tres_mas)>0){
                                                #           Update list of subgraphs CC>1 will be re-used in mst.knn
                                                acumulados=c(acumulados, nodos_tres_mas)
                                          }else{
                                                nodos_singletons=c(nodos_singletons, Filter(function(x) length(x)<=1, componente$subgraphs))
                                          }


                                    }

                                  }

                            }


                            #           Update list of subgraphs CC>=2 will be re-used in mst.knn. Only subgraphs have not been used in re-run of mst.knn
                            subgraphs=Filter(function(x) length(x)>=2, acumulados)

                            #           Stop criterion
                            if(length(subgraphs)<1) {cc=1}

                  }


                  # ----- Union of each subgraph with cc==1 ------ #

                  if(length(cluster_subgraphs)>0){

                        aristas_clusteres=lapply(cluster_subgraphs, function(x) igraph::get.edgelist(x))
                        aristas_clusteres <- do.call("rbind", aristas_clusteres)
                        aristas_clusteres <- unique(aristas_clusteres)
                        clusteres_unidos= igraph::graph.data.frame(d = aristas_clusteres[,1:2], directed = FALSE)
                        #clusteres_unidos=igraph::graph.union(cluster_subgraphs, byname = TRUE)

                  }else{

                        #           When there is nothing in cluster_subgraphs is because only singletons are yield.
                        cat("\n Only singletons are yielded \n")
                        clusteres_unidos=only.single.graphs(1:nrow(distance.matrix), nodos_singletons)

                  }



          ############################# END


                  # ----- Generate results ------ #

                  #Si estan todos los genes en clusteres unidos
                  if(length(igraph::V(clusteres_unidos)$name)==nrow(distance.matrix)){
                      resultados=generate.results(clusteres_unidos, distance.matrix)

                  }else{
                      cat("\n Only there is ", length(igraph::V(clusteres_unidos)$name), "nodes in solutions. Clustering solution only will have these nodes. \n")
                      g = clusteres_unidos
                      #Dejar en la matriz de distancia solo los nodos que están en la solución.
                      nodos.presentes=sort(as.numeric(igraph::V(clusteres_unidos)$name))
                      distance.matrix.solo.presentes=distance.matrix[nodos.presentes, nodos.presentes]

                      resultados=generate.results(g, distance.matrix.solo.presentes)

                  }



                 return(resultados)

    }else{
      stop("You should specify a distance object of class matrix or dataframe with equal numbers of rows and columns")
    }

  }else{

    stop("You should specify a distance object of class matrix or dataframe.")

  }




}

