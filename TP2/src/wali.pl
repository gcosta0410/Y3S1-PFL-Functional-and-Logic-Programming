:- use_module(library(lists)).
:- use_module(library(system)). %to use sleep/1
:- use_module(library(random)). %to use random_select/3

:- include('game.pl').
:- include('controllers.pl').
:- include('views.pl').
:- include('models.pl').
:- include('utils.pl').
:- include('testing.pl').

/*
* play/0
* Starts the game by displaying the start menu
*/
play:- cls, start_menu.






















