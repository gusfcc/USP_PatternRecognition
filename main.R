library(MASS)
library(class)
library(caret)

set.seed(6969)

#criar matriz binaria de cada uma das categorias escritas (tem ou não tem)
criarMatrizBinaria = function(dataset, nomeCol, elemento, novoNomeCol)
{
	colAux = matrix(0, nrow(dataset),1)
	
	for(i in 1:nrow(dataset)){
		if(grepl(elemento, dataset[i, nomeCol], fixed = TRUE)){
				colAux[i,] = 1;
		}
	}
	colnames(colAux) = novoNomeCol
	return(colAux)
}

steamCompleto = read.delim("steam.csv" , sep = "," , header = TRUE)

steamBinario = steamCompleto

#steam binario é o dataset com as informações binarias (tem ou não tem)
# dos dados dpo tipo string que estão no dataset

steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "platforms", "windows", "windows"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "platforms", "linux", "linux"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "platforms", "mac", "mac"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Free to Play", "free_to_play"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Action", "action"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Indie", "indie"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Adventure", "adventure"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Strategy", "strategy"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "RPG", "rpg"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Racing", "racing"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Simulation", "simulation"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Casual", "casual"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Sports", "sports"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Violent", "violent"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "genres", "Nudity", "nudity"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "categories", "Single-player", "single_player"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "categories", "Multi-player", "multiplayer"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "categories", "Steam Trading Cards", "steam_cards"))
steamBinario = data.frame(steamBinario, criarMatrizBinaria(steamCompleto, "developer", "Valve", "created_by_valve"))

steam = steamBinario


#for para retirar os jogos com informações erradas(mais horas jogadas que o jogo mais jogado da steam)
for(i in 1:27059){
	if(steam[i, "average_playtime"] > steamCompleto[23, "average_playtime"]){
		steam = steam[-i,]
	}
}

steamIDs = as.matrix(steam[,1])
steamNames = steam[,2]

labels = steam[,"owners"]

#retirar as colunas que não são úteis
steam = steam[, c(-1, -2 , -3, -5, -6, -7, -9, -10, -11, -17)]
rownames(steam) = steamIDs

steam = scale(steam) #Scale!!

porCent = 0.80
tamanho = nrow(steam)
treino = sample(1:tamanho, floor(porCent*tamanho))

labelsTreino = labels[treino]
steamTreino = steam[treino,]


KNN = knn(steamTreino, steam[-treino,], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])


LDA = lda(steamTreino, labelsTreino)
predito = predict(LDA, steam[-treino,])
confusionMatrix(predito$class, labels[-treino])


#For manual para analisar os dados depois de retirar 1 coluna de cada vez
#KNN(3)
i = 1

KNN = knn(steamTreino[,-i], steam[-treino,-i], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])
i = i + 1
i

#For manual para analisar os dados depois de retirar 1 coluna de cada vez
#KNN(7)
i = 1

KNN = knn(steamTreino[,-i], steam[-treino,-i], labelsTreino, 7)
confusionMatrix(KNN, labels[-treino])
i = i + 1
i

#For manual para analisar os dados depois de retirar 1 coluna de cada vez
#LDA
i = 1

LDA = lda(steamTreino[,-i], labelsTreino)
predito = predict(LDA, steam[-treino,-i])
confusionMatrix(predito$class, labels[-treino])
i = i + 1
i


#PerfectKNN3 - Colunas mais relevantes apos a analise com o KNN(3)
perfectKNN3 = c(-1, -3, -8, -9, -10, -11, -14, -15, -16, -17, -18, -20, -21, -22, -24, -25, -26, -27)

KNN = knn(steamTreino[, perfectKNN3], steam[-treino, perfectKNN3], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])

KNN = knn(steamTreino[, perfectKNN3], steam[-treino, perfectKNN3], labelsTreino, 7)
confusionMatrix(KNN, labels[-treino])

LDA = lda(steamTreino[,perfectKNN3], labelsTreino)
predito = predict(LDA, steam[-treino, perfectKNN3])
confusionMatrix(predito$class, labels[-treino])



#PerfectKNN7 - Colunas mais relevantes apos a analise com o KNN(7)
perfectKNN7 = c(-2, -3, -8, -13, -14, -15, -18, -19, -24, -25, -26)

KNN = knn(steamTreino[, perfectKNN7], steam[-treino, perfectKNN7], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])

KNN = knn(steamTreino[, perfectKNN7], steam[-treino, perfectKNN7], labelsTreino, 7)
confusionMatrix(KNN, labels[-treino])

LDA = lda(steamTreino[, perfectKNN7], labelsTreino)
predito = predict(LDA, steam[-treino, perfectKNN7])
confusionMatrix(predito$class, labels[-treino])



#PerfectLDA - Colunas mais relevantes apos a analise com o LDA
perfectLDA = c(-2, -9, -12, -13, -14, -15, -16, -17, -19, -21, -22, -23, -24, -25, -26, -27)

KNN = knn(steamTreino[,perfectLDA], steam[-treino, perfectLDA], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])

KNN = knn(steamTreino[, perfectLDA], steam[-treino, perfectLDA], labelsTreino, 7)
confusionMatrix(KNN, labels[-treino])

LDA = lda(steamTreino[,perfectLDA], labelsTreino)
predito = predict(LDA, steam[-treino, perfectLDA)
confusionMatrix(predito$class, labels[-treino])

#SuperPrefectKNN - Colunas mais relevantes de ambas análises do KNN - relação "e"
superPerfectKNN = c(-1, -2, -3, -8, -9, -10, -11, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -24, -25, -26, -27)

KNN = knn(steamTreino[,superPerfectKNN], steam[-treino, superPerfectKNN], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])

KNN = knn(steamTreino[, superPerfectKNN], steam[-treino, superPerfectKNN], labelsTreino, 7)
confusionMatrix(KNN, labels[-treino])

LDA = lda(steamTreino[,superPerfectKNN], labelsTreino)
predito = predict(LDA, steam[-treino, superPerfectKNN])
confusionMatrix(predito$class, labels[-treino])


#superPerfectAll - Colunas mais relevantes de todas as análises - relação "e"
superPerfectAll = c(-1, -2, -3, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, -25, -26, -27)

KNN = knn(steamTreino[,superPerfectAll], steam[-treino, superPerfectAll], labelsTreino, 3)
confusionMatrix(KNN, labels[-treino])

KNN = knn(steamTreino[, superPerfectAll], steam[-treino, superPerfectAll], labelsTreino, 7)
confusionMatrix(KNN, labels[-treino])

LDA = lda(steamTreino[,superPerfectAll], labelsTreino)
predito = predict(LDA, steam[-treino, superPerfectAll])
confusionMatrix(predito$class, labels[-treino])




