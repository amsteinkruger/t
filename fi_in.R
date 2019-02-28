# ---- fi_in ----
names = c("a",
          "b",
          "c")

real = c(1,
         2,
         3)

min = c(0.5,
        1,
        1.5)

max = c(2,
        4,
        6)

units = c("Unicorns",
          "Rabbits",
          "Biscuits")

fun = c("Unicorn Von Bertalanffy",
        "Rabbit Feed Conversion",
        "Breakfast Algorithm")

fi_in = data.frame(names, real, min, max, units)

write.csv(fi_in, "fi_in.csv")