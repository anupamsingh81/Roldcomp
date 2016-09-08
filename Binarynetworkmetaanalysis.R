library("gemtc")

head(SAE)
network <- mtc.network(data.ab=SAE[!SAE$treatment %in% c(21, 18,6,7),])# removes isolated groups in network
plot(network)
summary(network)


model <- mtc.model (network, type ="consistency", linearModel="random")

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
forest(relative.effect(result, "1"))
summary(relative.effect(result, "1", c("2", "30", "26")))