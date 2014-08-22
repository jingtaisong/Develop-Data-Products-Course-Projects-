library(xlsx)
library(igraph)
library(shiny)


##The list of names of national parks in the Southwest
park.list=c("Arches",
            "Big Bend", 
            "Black Canyon of the Gunnison",
            "Bryce Canyon", 
            "Canyonlands", 
            "Capitol Reef", 
            "Carlsbad Caverns", 
            "Channel Islands", 
            "Grand Canyon", 
            "Grand Teton", 
            "Great Basin", 
            "Great Sand Dunes",
            "Guadalupe Mountains", 
            "Joshua Tree", 
            "Kings Canyon", 
            "Lassen Volcanic", 
            "Mesa Verde", 
            "Petrified Forest", 
            "Redwood", 
            "Rocky Mountain", 
            "Saguaro", 
            "Sequoia", 
            "Yellowstone", 
            "Yosemite",
            "Zion")



##The data of distance among the 25 national parks in the list

distance<-c(0, 885, 180, 270, 26, 148, 662, 783, 308, 532, 329, 336, 696, 638, 866, 893, 129, 298, 1060, 317, 579, 831, 548, 713, 310, 885, 0, 829, 1075, 914, 1027, 242, 1161, 892, 1298, 1207, 679, 236, 944, 1332, 1657, 803, 701, 1789, 889, 624, 1296, 1359, 1372, 1029, 180, 829, 0, 397, 194, 275, 612, 909, 392, 509, 456, 207, 646, 765, 993, 1020, 154, 351, 1187, 222, 665, 958, 556, 840, 437, 270, 1075, 397, 0, 282, 112, 835, 588, 144, 570, 188, 591, 870, 444, 673, 714, 377, 378, 926, 532, 534, 637, 587, 519, 72, 26, 914, 194, 282, 0, 160, 649, 794, 295, 544, 341, 323, 683, 650, 878, 905, 116, 285, 1072, 329, 568, 843, 560, 725, 322, 148, 1027, 275, 112, 160, 0, 761, 679, 246, 520, 243, 468, 796, 535, 763, 759, 275, 379, 972, 410, 618, 728, 536, 610, 174, 662, 242, 612, 835, 649, 761, 0, 1011, 698, 1080, 989, 462, 36, 793, 1181, 1461, 538, 502, 1625, 672, 452, 1146, 1141, 1205, 787, 783, 1161, 909, 588, 794, 679, 1011, 0, 479, 1042, 615, 1000, 977, 199, 248, 573, 792, 626, 663, 1035, 549, 213, 1059, 289, 479, 308, 892, 392, 144, 295, 246, 698, 479, 0, 682, 297, 463, 733, 355, 631, 866, 255, 191, 1084, 624, 332, 595, 699, 610, 98, 532, 1298, 509, 570, 544, 520, 1080, 1042, 682, 0, 541, 647, 1130, 908, 1053, 851, 661, 829, 948, 434, 1073, 1101, 7, 812, 611, 329, 1207, 456, 188, 341, 243, 989, 615, 297, 541, 0, 646, 1020, 480, 573, 535, 453, 527, 731, 587, 703, 673, 553, 377, 199, 336, 679, 207, 591, 323, 468, 462, 1000, 463, 647, 646, 0, 497, 840, 1094, 1216, 208, 442, 1382, 256, 677, 1058, 666, 1036, 537, 696, 236, 646, 870, 683, 796, 36, 977, 733, 1130, 1020, 497, 0, 759, 1147, 1425, 573, 517, 1591, 706, 417, 1111, 1176, 1179, 822, 638, 944, 765, 444, 650, 535, 793, 199, 355, 908, 480, 840, 759, 0, 347, 675, 610, 423, 813, 900, 332, 311, 924, 375, 344, 866, 1332, 993, 673, 878, 763, 1181, 248, 631, 1053, 573, 1094, 1147, 347, 0, 403, 888, 721, 540, 1130, 721, 42, 1069, 112, 574, 893, 1657, 1020, 714, 905, 759, 1461, 573, 866, 851, 535, 1216, 1425, 675, 403, 0, 1022, 955, 214, 1093, 1045, 427, 868, 299, 724, 129, 803, 154, 377, 116, 275, 538, 792, 255, 661, 453, 208, 573, 610, 888, 1022, 0, 214, 1187, 401, 528, 850, 676, 841, 329, 298, 701, 351, 378, 285, 379, 502, 626, 191, 829, 527, 442, 517, 423, 721, 955, 214, 0, 1171, 587, 298, 683, 845, 697, 329, 1060, 1789, 1187, 926, 1072, 972, 1625, 663, 1084, 948, 731, 1382, 1591, 813, 540, 214, 1187, 1171, 0, 1335, 1164, 563, 964, 476, 1028, 317, 889, 222, 532, 329, 410, 672, 1035, 624, 434, 587, 256, 706, 900, 1130, 1093, 401, 587, 1335, 0, 968, 1099, 541, 981, 579, 579, 624, 665, 534, 568, 618, 452, 549, 332, 1073, 703, 677, 417, 332, 721, 1045, 528, 298, 1164, 968, 0, 684, 1087, 752, 487, 831, 1296, 958, 637, 843, 728, 1146, 213, 595, 1101, 673, 1058, 1111, 311, 42, 427, 850, 683, 563, 1099, 684, 0, 1092, 138, 538, 548, 1359, 556, 587, 560, 536, 1141, 1059, 699, 7, 553, 666, 1176, 924, 1069, 868, 676, 845, 964, 541, 1087, 1092, 0, 828, 627, 713, 1372, 840, 519, 725, 610, 1205, 289, 610, 812, 377, 1036, 1179, 375, 112, 299, 841, 697, 476, 981, 752, 138, 828, 0, 489, 310, 1029, 437, 72, 322, 174, 787, 479, 98, 611, 199, 537, 822, 344, 574, 724, 329, 329, 1028, 579, 487, 538, 627, 489, 0)

##Create an undirected weighted complete graph with the national parks as vertices and the distances as the weights of the edges

Park.graph<-graph.empty(directed=FALSE)+vertices(park.list)
Park.graph<-graph.adjacency(Park.Distance, mode="undirected",weighted=TRUE,diag=FALSE)


##This function is to match whether two vectors agree with each other on the first n positions. It's going to be used in the future
matchV<-function(vec1,vec2,n){
    x<-TRUE
    for (i in 1:n) {
        if (vec1[i]!=vec2[i]) {
            x<-FALSE
            break
        }
    }
    return (x)
}



##This is the heart function to compute the max_k shortedst paths from node_start to node_end in the graph

KshortestPath<-function(graph,node_start,node_end,max_k){
    distances <- shortest.paths(graph,v=node_start,weight=E(graph)$weight) ## this is the vector of shortest paths from node_start to all other nodes
    dijkpath.node <- get.shortest.paths(graph,from=node_start,to=node_end,weight=E(graph)$weight)$vpath[[1]] ## the vector of nodes on a shortest path
    dijkpath.node<-V(graph)$name[dijkpath.node]
    A<-list(list(distances[,node_end],dijkpath.node)) ## use a list to store a number (distances), a vector of character (path); A is a list of a list; currently A stores a single shortest path
    B<-list() ## B is currently an empty list to store the candidates for A
    
    if (length(A[[1]][[2]])==0) {return (A)} ## if no path from start to end return the current data stored in A
    if (max_k==1) {return (A)} ## from now on we assume max_k is at least 2
    
    for (k in 2:max_k){ ## we are find the k-th shortest paths, k starts from 2
        if (length(A[[k-1]][[2]]) < 2) next
        for (i in 1:(length(A[[k-1]][[2]])-1)) { ## search among the (k-1)-th shortest path, except for the last node
            node_spur <- A[[k-1]][[2]][i] ## the spur node is the i-th node on the path
            path_root <- A[[k-1]][[2]][1:i] ## the root path is the path up to the i-th node
            nodes.moved <- vector()
            edges.moved <- matrix(nrow=0,ncol=2) ## the matrix to store the two ends of the removed edges
            edges.moved.weight<-vector()
            for (path_k in 1:(k-1)) {## search among the previous paths stored in A
                cur_path <- A[[path_k]][[2]] ## look at the current path with nodes
                if (length(cur_path)>=i && matchV(cur_path[1:i], path_root, i)) {
                    edge_id<-get.edge.ids(graph,c(cur_path[i], cur_path[i+1])) 
                    if (edge_id!=0) {
                        edges.moved <- rbind(edges.moved, c(cur_path[i], cur_path[i+1])) ## store the edge to the list of removed edges
                        edge_weight<-E(graph)$weight[edge_id]
                        edges.moved.weight<-c(edges.moved.weight, edge_weight)
                        graph<-delete.edges(graph,edge_id) ##delete the edge from i to i+1; since the weights are assigned to E(g), the weight is deleted simultaneously
                    }
                }
            }
            if (i>1) {
                cur_edge_move<-E(graph)[inc(cur_path[1:(i-1)])] ## this returns the edge sequence
                cur_edge_move.weight<-cur_edge_move$weight ## this returns their weights
                M<-get.edges(graph,cur_edge_move)
                edges.moved.current<-t(apply(M,1,FUN=function(x) V(graph)$name[x])) ## this returns the edges in vertices
                edges.moved<-rbind(edges.moved,edges.moved.current) ## store the edges that are connected to the 1 to (i-1) verticex on the current path
                edges.moved.weight<-c(edges.moved.weight, cur_edge_move.weight) ## store the weights of those edges
                graph <- delete.vertices(graph, cur_path[1:(i-1)]) ## remove the root path except for the spur node; do nothing if i is just 1, this operations also removes all the related edges
                nodes.moved<-c(nodes.moved, cur_path[1:(i-1)])
            }
            path_spur<-get.shortest.paths(graph, from=node_spur, to=node_end, weight=E(graph)$weight)$vpath[[1]] ##get the shortest path from the spur node to the end node
            if (length(path_spur)!=0){ ##We must take the possibility that there is no such path into consider
                path_spur<-V(graph)$name[path_spur]
                dist_spur<-shortest.paths(graph, node_spur, to=node_end,weights=E(graph)$weight)
                if (length(path_spur)>0) {
                    path_total<-c(path_root,path_spur[-1])## join the root path with the spur path
                    dist_total<-as.numeric(distances[,node_spur] + dist_spur)
                    potential_k<-list(list(dist_total,path_total))## potential_k is the distance and the path of the path that we just found; it is the candidate of the kth shorted path
                    
                    ##search whether this path is already in the B list
                    search_potential<-FALSE
                    if (length(B) > 0) {
                        for (i.length.B in 1:length(B)) {
                            len<-length(B[[i.length.B]][[2]])
                            if (len!=length(path_total)) next
                            else {
                                search_potential<-matchV(B[[i.length.B]][[2]],path_total,len)
                                break
                            }
                        }
                    }
                    if (search_potential==FALSE) {
                        B<-c(B,potential_k)
                    }
                }
            }   
            
            ##below to restore the graph
            if (length(nodes.moved)>0){
                for (i.length in 1:length(nodes.moved)) {
                    graph <- graph + vertices(nodes.moved) ## add back the vertices
                }         
            }
            if (dim(edges.moved)[1]>0){
                for (i.edges in 1:dim(edges.moved)[1]) {
                    check1<-edges.moved[i.edges,1]
                    check2<-edges.moved[i.edges,2]
                    graph <- graph + edges(edges.moved[i.edges,1], edges.moved[i.edges,2]) ##add back the edges
                    edge_id <- get.edge.ids(graph, c(edges.moved[i.edges,1], edges.moved[i.edges,2]))
                    E(graph)$weight[edge_id]<-edges.moved.weight[i.edges]## add back the weight
                }
            }        
        }
        ##look for the paths in B with the shortest length and add it to A
        if (length(B)==0) {break}
        else {
            index_B<-1 
            dist_B<-B[[1]][[1]]
            for (i.B in 1:length(B)) {
                if (B[[i.B]][[1]] < dist_B) {
                    index_B<-i.B
                    dist_B<-B[[i.B]][[1]]
                }
            }
            A[[k]]<-B[[index_B]]
            B<-B[-index_B]
        }    
    }
    return (A)
}

## This is a function to pick the paths whose lengths exceed the shortest path by at most p percent
select.routes<-function(p,A) {
    n<-length(A)
    log<-logical()
    for (i in 1:length(A)) log<-c(log,(A[[i]][[1]]/A[[1]][[1]] <= 1+p))
    return (subset(A,log))
}

## Due to the limited computing power, we create a modified function KshortestPath.mod to filter among the 10 shortest paths to return the paths whose length exceed the shortest path at most by a certain percentage
KshortestPath.mod<-function(start,end,percentage){
    routes<-KshortestPath(Park.graph, start, end, 10)
    routes.sel<-select.routes(percentage/100,routes)
    return (routes.sel)
}

## This function prepares the output of KshortestPath.mod into the desired formats for the shiny output purposes
printRoute<-function(List.of.Routes) {
    n<-length(List.of.Routes)
    string<-character()
    if (n==0) return ("I am sorry, but the origin and the destination cannot be the same!")
    if (n>0) {
        for (i in 1:n){
            len<-length(List.of.Routes[[i]][[2]])
            if (len>=1) {
                string<-paste(string, "Route", sep="")
                string<-paste(string, i, sep=" ")
                string<-paste(string, ":", sep=" ")
                string<-paste(string, as.character(List.of.Routes[[i]][[2]][1]), sep=" ")
            }
            if (len>=2) {
                for (j in 2:len) {
                    string<-paste(string, as.character(List.of.Routes[[i]][[2]][j]), sep="->")
                }
            }
            string<-paste(string, ", the length of trip is: ", sep=" ")
            string<-paste(string, List.of.Routes[[i]][[1]], sep="")
            string<-paste(string, "miles.", sep=" ")
            string<-paste(string, "", sep="<br/>")
        }        
    }
    return (string)
}

## The server function for shiny
shinyServer(function(input, output) {
    output$Routes <- renderUI(
        HTML(printRoute(KshortestPath.mod(input$start,input$end,input$percentage)))
        )
})