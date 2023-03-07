#grupo 4
#tema Marvel
#integrantes;GABRIEL JOAQUIM CAMPOS 
#LUIZ FERNANDO RODRIGUES
#PEDRO DE OLIVEIRA GOMES CARNEIRO
#WU MING JU


#instalando igraph
install.packages('igraph')

#para usar a blibioteca

library(igraph)

#ler o arquivo de arestas

listas_arestas <-read.csv(file = "marvel-unimodal-edges.csv",
                         header = TRUE,sep = ',',
                         dec = ',')
#verficando se o arquivo e um data frame

is.data.frame(listas_arestas)

#olhar as primeiras linhas 

head(listas_arestas)

#ler o arquivo de vertices

listas_vertices <-read.csv(file = "marvel-unimodal-nodes.csv",
                          header = TRUE,sep = ',',
                          dec = ',')
#verficando se o arquivo e um data frame

is.data.frame(listas_vertices)

#olhar as primeiras linhas 

head(listas_vertices)

#construir o grafo

grafo <- graph_from_data_frame(listas_arestas,directed = FALSE,vertices = listas_vertices)

# plotando o grafo

tkplot(grafo,layout=layout.lgl, vertex.color="light green")

#atributos dos vertices

vertex_attr(grafo)

#atributo das arestas

edge_attr(grafo)

#grau descrente
sort(degree(grafo), decreasing = TRUE)[1:10]

#proximidade decresente
sort(closeness(grafo),decreasing = TRUE)[1:10]

#intermedidiacao decresente
sort(betweenness(grafo), decreasing = TRUE)[1:10]

#grau especifico
grau <- degree(grafo)
grau['Deadpool / Jack / Wade W']

#grau de valor menor ou maior
grau[grau>20]

#media interarestas
mean(betweenness(grafo))

#media grau
mean(grau)


#plot condicional
plot(grafo,
     vertex.label = ifelse(V(grafo)$name%in% c('Captain America','Wolverine / Logan'),V(grafo)$name,NA),
     vertex.size = ifelse(V(grafo)$name%in% c('Captain America','Wolverine / Logan'),30,7),
     vertex.color = ifelse(V(grafo)$name%in% c('Captain America','Wolverine / Logan'), "yellow",NA))


#centralidade
centralidade <-eigen_centrality(grafo)

#decrescente centralidade
sort(centralidade$vector, decreasing = TRUE)[1:10]

#vizinhos de jon 
vizinhos_CAP <- neighbors(grafo,'Captain America')
vizinhos_CAP   

#vizinhança vizinhança wanda
viz_wanda = neighborhood(grafo, order = 1, 'Scarlet Witch / Wanda')

#vizinhança de ordem 2 deadpool
viz_deadpool = neighborhood(grafo, order = 2,'Deadpool / Jack / Wade W')

#subgrafos wanda e phoenix mais poderosas quadrinho e filme
grafo_sub2 <- subgraph.edges(grafo,E(grafo)[inc(c('Scarlet Witch / Wanda', 'Phoenix Iii / Rachel S'))])

#plotando o subgrafo
tkplot(grafo_sub2, vertex.color = 'lightgreen', vertex.size=10)

#modularidade
comunidade2 <- cluster_fluid_communities(grafo,6)
modularity(comunidade2)

#membros da comunidade
membros_da_comunidade2 <- membership(comunidade2)
table(membros_da_comunidade2)

#ver a qual numero o vertice pertence
plot(comunidade2,grafo,vertex.label = membros_da_comunidade2, vertex.size = 5)

#vamos ver quem e o membro do grupo(grupo de nova york sempre tera 36)
comunidade_peterparker =membros_da_comunidade2[membros_da_comunidade2==6]
comunidade_peterparker

# distancia
distancia = distances(grafo)
distancia
# max distancia
max(distancia)
# histo
hist(stancia)
# diametro
diametro= diameter(grafo)
diametro
get_diameter(grafo)
