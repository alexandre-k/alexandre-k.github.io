---
title: Validating data from user input
author: Alexandre Krispin
date: Mon 10 Sep 2018 11:55:35 PM JST
tags: user input, text input, validate, clojurescript, reagent
---

# Validating user input in Reagent

Let's say we have a field where the user will enter some text. We want to make sure the first letter is entered in capital letter.

```
(def state (r/atom 0))

(defn change-state []
  [:div [:input {:type "button" :value "+1" :on-click #(swap! state inc)}]])

(defn change-title [user-movie]
  [:p "Change title: "
   [:input {:type "text" :on-change (fn [e] (let [val (-> e .-target .-value)] (swap! user-movie assoc :movie val)
                                      (println "Entered movie " val @user-movie)))}]])


```
