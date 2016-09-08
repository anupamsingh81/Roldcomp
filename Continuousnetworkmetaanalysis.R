library("gemtc")
#ContinuousOutcome


head(Acutemania3)
network <- mtc.network(data.ab=Acutemania3)
model <- mtc.model (network, type ="consistency", linearModel="random")

plot(network)
summary(network)




plot(model)
summary(model)
cat(model$code)
model$data

# run this model



result <- mtc.run(model, n.adapt=1000, n.iter=5000)

gelman.diag(result)


ranks <- rank.probability(result,preferredDirection=1)
print(ranks)
plot(ranks, beside=TRUE)
forest(relative.effect(result, "2"))
summary(relative.effect(result, "2", c("7", "9")))
