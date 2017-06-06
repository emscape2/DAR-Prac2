#leest csv files in afhankelijk van path

query <- read.csv("C:\\Users\\Debby Lam\\Documents\\UU jaar 2\\R\\query_product.csv",stringsAsFactors=FALSE)
description <- read.csv("C:\\Users\\Debby Lam\\Documents\\UU jaar 2\\R\\product_descriptions.csv", stringsAsFactors = FALSE)

total <- merge(query, description, by="product_uid")

#Sla hele kolom van producttitel van data op in variabele
product <- query[,3]  

#Sla hele kolom van search terms op in variabele
searchTerm <- query[,4] 

relevanceScore <- query[,5]

# telt aantal unique values voorkomens van product
productTable <- table(product)  

productDescription <- description[,2]

# geeft het aantal unieke values in product terug
length(unique(product)) 

length(unique(productDescription))

length(unique(searchTerm))

# geeft wat statistieken van de relevantie-score

summary(query$relevance)

# laad de libraries tau en tm (eerst installeren!)
library(tau)
library(tm)
library(MASS)
library(nnet)

#gemiddelde van relevantiescore van query_product
meanScore <- mean(relevanceScore)

#standaard deviatie van relevantieScore 
sdScore <- sd(relevanceScore)

#maakt x-as range
x <- seq(0,4, by = 0.05)
  
#maakt y-as range mbv normaal verdeling functie
y <- dnorm(x, meanScore, sdScore)

#maakt grafiek van  een normaal verdeling van de Relevantie score
plot(x,y, col="blue",xlab="Relevantie Score", ylab="", type="h",lwd=2, cex=2,cex.axis=.8)

#maakt histogram van reevancescore maar geeft niet alle histogrammen weer
hist(relevanceScore, breaks = seq(1,3, by= 0.3), col = "lightblue", ylim = c(0,30000))

#geeft aantal voorkomens van relevantie score terug
table(relevanceScore)

# Bekijk de productnaam van het 1ste query-product paar

query[1,3]
total[1,3]

# verwijder getallen uit de productnaam

query[1,3] <- removeNumbers(query[1,3])
query[1,3]

# verwijder leestekens uit de productnaam

query[1,3] <- removePunctuation(query[1,3])
query[1,3]


#verwijdert punctuatie en decapitalizes 
product <- removePunctuation(product)
product <- tolower(product)

searchTerm <- removePunctuation(searchTerm)
searchTerm <- tolower(searchTerm)

stopwords <- c("a","able","about", "above", "abst", "accordance", "according", "accordingly", "across", "act", "actually", "added", "adj",
  "affected","affecting","affects","after", "afterwards", "again", "against", "ah", "all", "almost", "alone", "along", "already",
  "also", "although", "always", "am", "among", "amongst", "an", "and", "announce", "another", "any", "anybody", "anyhow", "anymore",
  "anyone", "anything", "anyway", "anyways", "anywhere", "apparently", "approximately", "are", "aren", "arent", "arise", "around",
  "as", "aside", "ask", "asking", "at", "auth", "available", "away", "awfully", "b", "back", "be", "became", "because", "become",
  "becomes", "becoming", "been", "before", "beforehand", "begin", "beginning", "beginnings", "begins", "behind", "being", "believe",
  "below", "beside", "besides", "between", "beyond", "biol", "both", "brief", "briefly", "but", "by", "c", "ca", "came", "can",
  "cannot", "can't", "cause", "causes", "certain", "certainly", "co", "com", "come", "comes", "contain", "containing", "contains",
  "could", "couldnt", "d", "date", "did", "didn't", "different", "do", "does", "doesn't", "doing", "done", "don't", "down", "downwards",
  "due", "during", "e", "each", "ed","edu", "effect","eg","eight", "eighty","either","else","elsewhere","end","ending", "enough",
  "especially","et","et-al","etc","even","ever","every","everybody","everyone","everything","everywhere", "ex", "except", "f",
  "far", "few", "ff","fifth","first","five","fix","followed","following","follows","for","former","formerly", "forth","found",
  "four","from", "ft","further","furthermore","g","gave","get", "gets","getting","give","given","gives","giving","go","goes", "gone",
  "got", "gotten", "h", "had", "happens","hardly","has","hasn't","have","haven't","having","he","hed","hence","her", "here",
  "hereafter","hereby","herein","heres","hereupon","hers","herself","hes","hi","hid","him","himself","his","hither","home",
  "how","howbeit","however","hundred","i","id","ie","if","i'll","im","immediate","immediately","importance","important",
  "in","inc","indeed","index","information","instead","into","invention","inward","is","isn't","it","itd","it'll","its","itself",
  "i've","j","just","k","keep","keeps","kept","kg","km","know","known", "knows","l","largely","last","lately","later","latter",
  "latterly","least","less","lest","let","lets","like","liked","likely","line","little","'ll","look","looking","looks","ltd","m",
  "made","mainly","make","makes","many","may","maybe","me","mean","means","meantime","meanwhile","merely","mg","might","million",
  "miss","ml","more","moreover","most","mostly","mr","mrs","much","mug","must","my","myself","n","na","name","namely","nay", "nd",
  "near", "nearly","necessarily","necessary","need","needs","neither","never","nevertheless","new", "next", "nine", "ninety",
  "no", "nobody", "non", "none", "nonetheless", "noone", "nor", "normally","nos", "not","noted", "nothing", "now", "nowhere",
  "o", "obtain", "obtained", "obviously", "of", "off", "often", "oh", "ok", "okay", "old", "omitted", "on", "once","one",
  "ones", "only", "onto","or", "ord","other", "others", "otherwise", "ought", "our","ours", "ourselves",  "out", "outside",
  "over", "overall","owing","own","p","page","pages","part","particular","particularly","past","per","perhaps","placed",
  "please","plus","poorly","possible","possibly","potentially","pp","predominantly","present","previously","primarily",
  "probably","promptly","proud","provides","put","q","que","quickly","quite","qv","r","ran","rather","rd","re","readily",
  "really","recent","recently","ref","refs","regarding","regardless","regards","related","relatively","research",
  "respectively", "resulted","resulting","results","right","run","s","said","same","saw","say","saying","says","sec",
  "section","see","seeing","seem","seemed","seeming","seems","seen","self","selves","sent","seven","several", "shall",
  "she", "shed","she'll","shes","should","shouldn't","show","showed","shown","showns","shows","significant","significantly",
  "similar","similarly","since","six","slightly","so","some","somebody","somehow","someone","somethan","something",
  "sometime","sometimes","somewhat","somewhere","soon","sorry","specifically","specified","specify","specifying",
  "still","stop","strongly","sub","substantially","successfully","such","sufficiently","suggest","sup","sure", "t",
  "take","taken","taking","tell","tends","th","than","thank","thanks","thanx","that","that'll","thats","that've",
  "the","their","theirs","them","themselves","then","thence","there","thereafter","thereby","thered","therefore",
  "therein","there'll","thereof","therere","theres","thereto","thereupon","there've","these","they","theyd", "they'll",
  "theyre","they've","think","this","those","thou","though","thoughh","thousand","throug","through","throughout",
  "thru","thus","til","tip","to","together","too","took","toward","towards","tried","tries","truly","try","trying",
  "ts","twice","two","u","un","under","unfortunately","unless","unlike","unlikely","until","unto","up","upon",
  "ups", "us","use","used","useful","usefully","usefulness","uses","using","usually","v","value","various","'ve",
  "very","via","viz","vol","vols","vs","w","want","wants","was","wasnt","way","we","wed","welcome","we'll",
  "went","were","werent","we've","what","whatever","what'll","whats","when","whence","whenever","where","whereafter","whereas",
  "whereby","wherein","wheres","whereupon","wherever","whether","which","while","whim","whither","who","whod","whoever",
  "whole","who'll","whom","whomever","whos","whose","why","widely","willing","wish","with","within","without","wont",
  "words","world","would","wouldnt","www","x","y","yes","yet","you","you'd","you'll","your","you're","yours","yourself",
  "yourselves", "you've", "z", "zero")


description[,2] <- removePunctuation(description[,2])
description[,2] <- tolower(description[,2])
description[,2] <- removeWords(description[,2],stopwords)
description[,2]

b <- textcnt(description[,2], method="string", n=1L)


# bepaal welke woorden voorkomen (en hoe vaak) in de productnaam

a <- textcnt(product,method="string",n=1L)
a

# welke woorden komen voor?
names(a)

# functie om te berekenen of alle querywoorden in de productnaam voorkomen

all.queryterms <- function (queries,docs) 
{
  n <- length(queries)
  feature <- vector(length=n)
  for(i in 1:n){
    query <- queries[i]
    document <- docs[i]
    a <- textcnt(query,method="string",n=1L)
    b <- textcnt(document,method="string",n=1L)
    c <- intersect(names(a), names(b))
    feature[i] <- as.numeric(length(a)==length(c))}
  feature
}


query$search_term <- tolower(query$search_term)
query$product_title <- tolower(query$product_title)


allTerm <- all.queryterms(query$search_term, query$product_title)
allDesc <- all.queryterms(total$search_term, total$product_description)

# in ongeveer 24% van de query-product paren komen alle zoektermen voor in de productnaam

summary(allTerm)

#In ongeveer 22% van de query-description paren komen alle zoektermen in beschrijving voor
summary(allDesc)

# maak een data frame met de zojuist berekende feature en de relevantie-score.

qp <- data.frame(relevance=query$relevance,allTerm=allTerm)
dp <- data.frame(relevance=total$relevance, allDesc=allDesc)

# trek een random sample ter grootte 50000 uit de getallen 1:74067
# dit zijn de rij-nummers van de trainig set

tr.index <- sample(74067,50000)

# schat een lineair regressiemodel op de trainingset

qp.lm <- lm(relevance~allTerm,data=qp[tr.index,])
dp.lm <- lm(relevance~allDesc,data=dp[tr.index,])

# bekijk het model; heeft de coefficient van "allterms" het verwachtte teken?

summary(qp.lm)
summary(dp.lm)




