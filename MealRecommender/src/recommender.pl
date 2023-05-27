%%%%%%%%%%%
%%% NLP %%%
%%%%%%%%%%%

%% Parsing %%

sentence(s(VP)) --> verb_phrase(VP).
sentence(s(VP)) --> verb_phrase(VP), dot.


noun_phrase(np(Adjp, Noun)) --> adj_phrase(Adjp), noun(Noun).
noun_phrase(np(Det,Noun))--> det(Det),noun(Noun).
noun_phrase(np(Noun))--> noun(Noun).
noun_phrase(np(Noun,Conj,NP)) --> noun(Noun),conj(Conj),noun_phrase(NP).

verb_phrase(vp(Propnoun,Verb, NP)) --> propnoun(Propnoun),verb(Verb), noun_phrase(NP).
verb_phrase(vp(Propnoun,Verb, Det,NP)) --> propnoun(Propnoun),verb(Verb),det(Det), noun_phrase(NP).

adj_phrase(adjp(Adj)) --> adj(Adj).
adj_phrase(adjp(Comma,Adjp))-->comma(Comma),adj_phrase(Adjp).
adj_phrase(adjp(Conj,Adjp)) --> conj(Conj),adj_phrase(Adjp).
adj_phrase(adjp(Adj, Adjp)) --> adj(Adj), adj_phrase(Adjp).

conj(c(and))-->[and].
verb(v(want)) --> [want].
det(d(a)) --> [a].
propnoun(pn(i))-->[i].
comma(comma(',')) -->[',']. 

%%adj(a()) --> [].
adj(adj(spicy)) --> [spicy].
adj(a(fastfood)) --> [fastfood].
adj(a(sitdown)) --> [sitdown].
adj(a(delivery)) --> [delivery].
adj(a(cheap)) --> [cheap].
adj(a(average)) --> [average].
adj(a(expensive)) --> [expensive].


%%noun(n()) -->[].
noun(n(food)) -->[food].
noun(n(american)) -->[american].
noun(n(burger)) -->[burger].
noun(n(wings)) -->[wings].
noun(n(steak)) -->[steak].
noun(n(beef)) -->[beef].
noun(n(chinese)) -->[chinese].
noun(n(rice))-->[rice].
noun(n(chicken))-->[chicken].
noun(n(noodles)) -->[noodles].
noun(n(dumplings)) -->[dumplings].
noun(n(japanese)) -->[japanese].
noun(n(sushi)) -->[sushi].
noun(n(seafood)) -->[seafood].
noun(n(pizza)) -->[pizza].
noun(n(pasta)) -->[pasta].
noun(n(tacos)) -->[tacos].
noun(n(burritos)) -->[burritos].


dot --> ['.'].
dot -->[].



%%%%%%%%%%%%%%%%%%%%%%
%%%   Model Code   %%%
%%%%%%%%%%%%%%%%%%%%%%
print(Categories,Mode,Price,Nouns):-
    write("Categories:"),write(Categories),nl,
    write("Mode:"),write(Mode),nl,
    write("Price:"),write(Price),nl,
    write("Nouns: "),write(Nouns),nl.
print_KB(KB):-
        findall(OC,member(category(OC),KB),[OldCategory]),write("OldCategory:"),write(OldCategory),nl,
        findall(ON,member(nouns(ON),KB),[OldNouns]),write("OLDNouns:"),write(OldNouns),nl,
        findall(OP,member(price(OP),KB),[OldPrice]),write("OldPrice: "),write(OldPrice),nl,
        findall(OM,member(mode(OM),KB),[OldMode]),write("OldMode: "),write(OldMode),nl.

%%%%%%%%%%%%%
%%Main Loop%%
%%%%%%%%%%%%%

%%Initial case


start():-knowledge_base(KB),write("What would you like to eat?:"),nl,
    read_word_list(In),extract_words(In,List),sort_words(List,Categories,Nouns,Price,Mode),
    select_res(Categories,Nouns,Price,Mode,KB,Selected,Category),write("Do you want: "),write(Selected),nl,
    append(KB,[previous(Selected),nouns(Nouns),category(Category),mode(Mode),price(Price)],NewKB),
    write("If this is not what you want type a new statment else type done"),nl,read_word_list(NEWIN),revision(NEWIN,NewKB).
start():-write("FAILED TO PARSE INPUT TRY AGAIN").
%%%%Initiate debug mode

%%%%%%%%%%%%%%
%% Revision %%
%%%%%%%%%%%%%%

revision(In,KB):-In=[Head|_],Head=done.
revision(In,KB):-In=[Head|_],Head=debug,write("Enter Debug Mode"),nl,write("INPUT: "),nl,read_word_list(NEWIN),revision_debug(NEWIN,KB).

revision_debug(In,KB):-In=[Head|_],Head=done.

revision_debug(In,KB):-In=[Head|_],Head=printkb,print_KB(KB),nl,
    write("write new command"),nl,read_word_list(NEWIN),revision_debug(NEWIN,KB).
revision_debug(In,KB):-In=[Head|_],Head=getprevious,findall(P,member(previous(P),KB),OS),write("OS: "),write(OS),nl,
    write("write new command"),nl,read_word_list(NEWIN),revision_debug(NEWIN,KB).

%%%For revision process normal then revise the categories,price,and mode checking for new information
%%%Then combine the old and new nouns and make a recommendation
%%(The nouns wont affect the category since we selected it in category revision and put it into the categories list)

revision_debug(In,KB):-
    write("Redo"),nl,print_KB(KB),extract_words(In,List),sort_words(List,Categories,Nouns,Price,Mode),print(Categories,Mode,Price,Nouns),
    category_revision(KB,Nouns,Category,Categories),
    price_revision(KB,Price,NewPrice),
    mr(KB,Mode,NewMode),
    findall(OS,member(previous(OS),KB),OldSelections),
    findall(ON,member(nouns(ON),KB),[OldNouns]),append(Nouns,OldNouns,AllNouns),write("Input to selectres:"),nl,print([Category],NewMode,NewPrice,AllNouns),
    select_res([Category],AllNouns,NewPrice,NewMode,KB,Selected,OutCategory,OldSelections),write(Selected),
    append(OldSelections,[Selected],AllSelections),
    memoryrevision(AllNouns,MemoryNouns),
    knowledge_base(KB2),append(KB2,[previous(AllSelections),nouns(MemoryNouns),category(Category),mode(NewMode),price(NewPrice)],NewKB),
    nl,write("If this is not what you want type a new statment else type done"),nl,read_word_list(NEWIN),revision_debug(NEWIN,NewKB).

    memoryrevision(Words,NewWords):-length_list(Count,0,Words),Count<7,NewWords = Words.
    memoryrevision(Words,NewWords):-drop_last(Words,ShorterList),memoryrevision(ShorterList,NewWords).
%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Non debug mode%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
revision(In,KB):-
    extract_words(In,List),sort_words(List,Categories,Nouns,Price,Mode),
    category_revision(KB,Nouns,Category,Categories),
    price_revision(KB,Price,NewPrice),
    mr(KB,Mode,NewMode),
    findall(OS,member(previous(OS),KB),OldSelections),
    findall(ON,member(nouns(ON),KB),[OldNouns]),append(Nouns,OldNouns,AllNouns),
    select_res([Category],AllNouns,NewPrice,NewMode,KB,Selected,OutCategory,OldSelections),write("Do you want: "),write(Selected),write("?"),
    append(OldSelections,[Selected],AllSelections),
    memoryrevision(AllNouns,MemoryNouns),
    knowledge_base(KB2),append(KB2,[previous(AllSelections),nouns(MemoryNouns),category(Category),mode(NewMode),price(NewPrice)],NewKB),
    nl,write("If this is not what you want type a new statment else type done"),nl,read_word_list(NEWIN),revision(NEWIN,NewKB).

    memoryrevision(Words,NewWords):-length_list(Count,0,Words),Count<8,NewWords = Words.
    memoryrevision(Words,NewWords):-drop_last(Words,ShorterList),memoryrevision(ShorterList,NewWords).

    

%%%%%%%%%%%%%%%%%
%%Price Revision%
%%%%%%%%%%%%%%%%%

%%Look at new and old price info to generate the new price info
%%No new price use old
price_revision(KB,Price,NewPrice):-Price=[],findall(X,member(price(X),KB),[OldPrice]),NewPrice = OldPrice.
%%If a price does exist then use that instead of the old
price_revision(_,Price,NewPrice):- NewPrice=Price.

%%%%%%%%%%%%%%%%
%%Moderevision%%
%%%%%%%%%%%%%%%%

%%no new mode use old
mr(KB,Mode,NewMode):-Mode=[],findall(X,member(mode(X),KB),[NewMode]).
%% new mode use that in place of the old
mr(KB,Mode,NewMode):-NewMode is Mode.

%%%%%%%%%%%%%%%%%%%%%
%%Category revision%%
%%%%%%%%%%%%%%%%%%%%%

%%Look into the previous decision and see if the new nouns line up with the old category

%%if a category is given then use that 
category_revision(_,_,Category,Categories):- \+Categories =[],length(Categories,1),Categories=[Category|_].

%%If no new nouns were provided stick with the origional category
category_revision(KB,Nouns,Category,_):-Nouns=[],findall(OC,member(category(OC),KB),[Category]).

%%If there are new nouns check if the noun is in the same category
category_revision(KB,Nouns,Category,_):- \+Nouns = [], features_cats(Temp),findall(X,member(category(X),KB),[Previous]),Temp=[[Previous,Prev_Features]|_],contains(Nouns,Prev_Features),Category = Previous.
    
%%nouns dont all belong in the new catagory so re-run recommendation with the new and old categories
category_revision(KB,Nouns,Category,Categories):- findall(X,member(nouns(X),KB),[OldNouns]),append(Nouns,OldNouns,AllNouns),
    category_selection(Nouns,Category,_).

%%%%%%%%%%%%%%%%%%%%%%%
%%Recommendation loop%%
%%%%%%%%%%%%%%%%%%%%%%%

%%Handeler for the multiple selection loops returns the category as well as the top element of the recommendation list
select_res(Categories,Nouns,Price,Mode,KB,Selected,Category):-selectloop(Categories,Nouns,Price,Mode,KB,List,Category),(\+Nouns=[],getshortlist(Nouns,KB,List,Options);Nouns=[],Options=List),priceloop(Price,KB,Options,Refined),modeloop(Mode,KB,Refined,Final),(Nouns=[],Final=[Selected|_];\+Nouns=[],fc(KB,Final,Nouns,0,Choice,Selected)).
%%For revisions modify the selection
select_res(Categories,Nouns,Price,Mode,KB,Selected,Category,Previous):-
    selectloop(Categories,Nouns,Price,Mode,KB,List,Category),
    (\+Nouns=[],getshortlist(Nouns,KB,List,Options);
        Nouns=[],Options=List),
        priceloop(Price,KB,Options,Refined),
        modeloop(Mode,KB,Refined,Final),
        (Nouns=[],Final=[Selected|_];
            \+Nouns=[],fc(KB,Final,Nouns,0,Choice,Possible)),check(Final,Previous,Possible,Selected).


%%Quick check to see if any options have been selected before
check(Final,Previous,Possible,Selected):- (length(Final,1); \+member(Possible,Previous)),Selected = Possible.
check(Final,Previous,Possible,Selected):- Final = [Head|Tail],check(Tail,Previous,Head,Selected).
%%%%%%%%%%%%%%%%%%%%%
%%Initial selection%%
%%%%%%%%%%%%%%%%%%%%%

%%Check to see if a category was used in the input if not then run category_selection to get the category also if multiple categories were selected return false
selectloop(Categories,Nouns,Price,Mode,KB,List,Category):-
    Categories = [],
    category_selection(Nouns,ResultCat,ResultScore),write("Selected Category: "),write(ResultCat),nl,
    findall(Restraunt,member(is(Restraunt,ResultCat),KB),List),Category=ResultCat.

selectloop(Categories,Nouns,Price,Mode,KB,List,Category):-
    \+Categories = [],length(Categories, 1),Categories=[ResultCat|_],
    findall(Restraunt,member(is(Restraunt,ResultCat),KB),List),Category=ResultCat.

selectloop(Categories,Nouns,Price,Mode,KB,List,Category):-
    \+Categories = [],\+length(Categories, 1),write("Multiple categories selected please refine to one categories"),false.

%%%%%Get a list of Restraunts that fits the nouns provided%%%%%
getshortlist(Nouns,KB,List,Options):-
    Nouns=[],NewList = [].

getshortlist(Nouns,KB,List,Options):-
    Nouns = [Word|Tail],findall(Next,(member(has(Next,Word),KB),member(Next,List)),Bag),getshortlist(Tail,KB,List,NewOptions),append(NewOptions,Bag,Options).

%%%%%%%%%%%%%
%%Price lop%%
%%%%%%%%%%%%%


%%Check the price list if none was selected assume normal if more then one were selected return false and ask user to specify
%%If the price would result in an empty list ignore it in favor of noun selection
priceloop(Price,KB,Options,Refined):-
    Price=[],SelectedPrice=normalprice,findall(Restraunt,(member(is(Restraunt,normalprice),KB),member(Restraunt,Options)),Bag),(Bag=[],Refined=Options;\+Bag=[],Refined = Bag).
priceloop(Price,KB,Options,Refined):-
    \+Price=[],length(Price,1),Price=[Selected|_],findall(Restraunt,(member(is(Restraunt,Selected),KB),member(Restraunt,Options)),Bag),(Bag=[],Refined=Options;\+Bag=[],Refined = Bag).
priceloop(Price,_,_,_):-
    \+Price=[],\+length(Price,1),write("To many price indicators please refine"),false.

%%%%%%%%%%%%%
%%Mode loop%%
%%%%%%%%%%%%%

%%Same rules as the price loop just uses mode and assumes fastfood 
modeloop(Mode,KB,Options,Refined):-
    Mode=[],SelectedMode=fastfood,findall(Restraunt,(member(is(Restraint,SelectedMode),KB),member(Restraunt,Options)),Bag),(Bag=[],Refined=Options;\+Bag=[],Refined = Bag).

modeloop(Mode,KB,Options,Refined):-
    \+Mode=[],length(Mode,1), Mode=[SelectedMode|_],findall(Restraunt,(member(is(Restraint,SelectedMode),KB),member(Restraunt,Options)),Bag),(Bag=[],Refined=Options;\+Bag=[],Refined = Bag).

modeloop(Mode,KB,Options,Refined):-
   \+Mode=[],\+length(Mode,1),write("To many modes selected please refine"),false.

%%%%%%%%%%%%%%%
%%Final Check%%
%%%%%%%%%%%%%%%
fc(KB,Options,Nouns,BS,Choice,Out):-Options=[],Out = Choice.
fc(KB,Options,Nouns,BS,Choice,Out):-Options=[Selected|Tail],scoregen(Selected,Nouns,KB,0,Score),Score>BS,NewBS = Score,NewChoice = Selected,fc(KB,Tail,Nouns,NewBS,NewChoice,Out).
fc(KB,Options,Nouns,BS,Choice,Out):-Options=[Selected|Tail],scoregen(Selected,Nouns,KB,0,Score),\+Score>BS,NewBS is BS,NewChoice =Choice,fc(KB,Tail,Nouns,NewBS,NewChoice,Out).

scoregen(Target,Nouns,KB,Score,Out):-Nouns=[],Out = Score.
scoregen(Target,Nouns,KB,Score,Out):- Nouns = [Word|Rest],member(has(Target,Word),KB),NewScore is Score + 1,scoregen(Target,Rest,KB,NewScore,Out).

scoregen(Target,Nouns,KB,Score,Out):- Nouns = [Word|Rest],\+member(has(Target,Word),KB),NewScore is Score,scoregen(Target,Rest,KB,NewScore,Out).

%%%%%%%%%%%
%%%Utils%%%
%%%%%%%%%%%
drop_last([X|Xs],Ys):-
    drop(Xs,Ys,X).
drop([],[],_).
drop([X1|Xs],[X0|Ys],X0):-
    drop(Xs,Ys,X1).

length_list(L,X,List):-List=[],L is X.
length_list(L,X,[_|T]):-length_list(L,X+1,T).

contains(List,Source):-List=[].
contains(List,Source):-List=[Head|Tail],member(Head,Source),contains(Tail,Source).

%%Sorts words into the various categories used by the selection process
sort_words(List,Categories,Nouns,Price,Mode):-
    List = [],Nouns=[],Price=[],Categories=[],Mode=[].
sort_words(List,Categories,Nouns,Price,Mode):-
    List = [Word|Tail],categories_list(State),member(Word,State),sort_words(Tail,NewCategories,Nouns,Price,Mode),Categories = [Word|NewCategories].
sort_words(List,Categories,Nouns,Price,Mode):-
    List = [Word|Tail],nouns_list(State),member(Word,State),sort_words(Tail,Categories,NewNouns,Price,Mode),Nouns = [Word|NewNouns].
sort_words(List,Categories,Nouns,Price,Mode):-
    List = [Word|Tail],price_list(State),member(Word,State),sort_words(Tail,Categories,Nouns,NewPrice,Mode),Price = [Word|NewPrice].
sort_words(List,Categories,Nouns,Price,Mode):-
    List=[Word|Tail],mode_list(State),member(Word,State),sort_words(Tail,Categories,Nouns,Price,NewMode),Mode=[Word|NewMode].
sort_words(List,Categories,Nouns,Price,Mode):-
    List=[Word|Tail],sort_words(Tail,Categories,Nouns,Price,Mode).
%%Reads words from the users input
read_word_list(Ws) :-
    read_line_to_codes(user_input,Cs),
    atom_codes(A,Cs),
    tokenize_atom(A,Ws).

%%Handels listening for and parsing the users input into a list of words to be used for recommendation
parse(List):-write("Enter what you want to eat"),nl,read_word_list(IN), extract_words(IN,List).

%%%%%%%%%%%%%%%%%
%%Compare Nouns%%
%%%%%%%%%%%%%%%%%

%%%%Handels the category selection from a list of key words
category_selection(List,ResultCat,ResultScore):-features_cats(SampleList),Score = 0,get_cat(List,american,Score,SampleList,ResultCat,ResultScore).

%%Runs a comparison loop for each of the categories keeping track of the best category and score in case of a tie it keeps the initial choice
get_cat(_,BestCategory,BestScore,AllCategories,ResultCat,ResultScore):-
    AllCategories=[],ResultScore = BestScore,ResultCat=BestCategory.

get_cat(List,BestCategory,BestScore,AllCategories,ResultCat,ResultScore):-
    Score = 0,
    AllCategories=[Selected|Tail],
    Selected = [TrialCategory,TrialFeatures|_],
    compare_features(List,TrialCategory,TrialFeatures,Score,BestScore,BestCategory,Out,NewBestScore),
    get_cat(List,Out,NewBestScore,Tail,ResultCat,ResultScore).
%%%Runs a comparison from the words used to identify a category to the words provided
%%Base case for compare
compare_features(List,TrialCategory,_,Score,BestScore,BestCategory,Out,NewBestScore):- 
    List = [],Score > BestScore, Out = TrialCategory,NewBestScore is Score.
compare_features(List,TrialCategory,_,Score,BestScore,BestCategory,Out,NewBestScore):- 
    List = [],Score is BestScore, Out = BestCategory,NewBestScore is Score.
compare_features(List,TrialCategory,_,Score,BestScore,BestCategory,Out,NewBestScore):- 
    List = [],Score < BestScore, Out = BestCategory, NewBestScore is BestScore.
%%Check the first element of the list and pass in the next
compare_features(List,TrialCategory,TrialFeatures,Score,BestScore,BestCategory,Out,NewBestScore):-
    List = [Feature|Tail],
    member(Feature,TrialFeatures),NewScore is Score + 1,
    compare_features(Tail,TrialCategory,TrialFeatures,NewScore,BestScore,BestCategory,Out,NewBestScore).
compare_features(List,TrialCategory,TrialFeatures,Score,BestScore,BestCategory,Out,NewBestScore):-
    List = [Feature|Tail],
    \+member(Feature,TrialFeatures),NewScore is Score,
    compare_features(Tail,TrialCategory,TrialFeatures,NewScore,BestScore,BestCategory,Out,NewBestScore).
%%%%%%%%%%%%%%%%%
%%%% Extract %%%%
%%%%%%%%%%%%%%%%%

%%Extract a noun list needs to be refined to accept adj's
extract_words(In,List):- sentence(Parse,In,[]),extract(Parse,NP),retrieve([NP],List).

%%Full sentence rules
extract(s(vp(pn(i),v(want), NounPhrase)),NounPhrase).
extract(s(vp(pn(i),v(want),d(a), NounPhrase)),NounPhrase).

%%Noun Phrases rules
extract(np(adjp(Adjp),n(Noun)),Adjp,Noun).
extract(np(adjp(adj(Adj),Adjp),n(Noun)),Adj,Adjp,Noun).

%%Adjp rules
extract(adjp(adj(Adj)),Adj).
extract(adjp(comma(,),adjp(Adjp)),Adjp).
extract(adjp(c(and),adjp(Adjp)),Adjp).

%Adj
extract(adj(Adj),Adj).
extract(a(Adj),Adj).


%%Get nouns from
extract(np(det(a),n(Noun)),Noun).
extract(np(n(Noun)),Noun).
extract(np(n(Noun),c(and),NounPhrase),Noun,NounPhrase).


%%%%%%%%%%%%%%%%
%Noun Retrieval%
%%%%%%%%%%%%%%%%
%%Base case
retrieve(NounPhrases,Out):- NounPhrases=[],Out=[].
%%noun is by its self
retrieve(NounPhrases,Out):-
   NounPhrases=[Noun|Tail],
   \+extract(Noun,_),\+extract(Noun,_,_),\+extract(Noun,_,_,_),
   retrieve(Tail,NewList),Out=[Noun|NewList].

retrieve(NounPhrases,Out):- 
    NounPhrases=[NP|Tail],extract(NP,NewNp1,NewNp2,NewNp3),
    append([NewNp1],Tail,Temp1),append([NewNp2],Temp1,Temp2),append([NewNp3],Temp2,Temp3),
    retrieve(Temp3,Out).
%%two noun phrases inside an np
retrieve(NounPhrases,Out):-
    NounPhrases=[NP|Tail],extract(NP,NewNp1,NewNp2),append([NewNp1],Tail,Temp1),append([NewNp2],Temp1,Temp2),retrieve(Temp2,Out).
%%one noun phrase inside an np
retrieve(NounPhrases,Out):-
    NounPhrases=[NP|Tail],extract(NP,NewNp),append([NewNp],Tail,Temp1),retrieve(Temp1,Out).


%%%%%%%%%%%%%%%%%%%%%%
%%Knowledge base form%
%%%%%%%%%%%%%%%%%%%%%%

%%Features of categories in the form of a list with [Category,[features]]
features_cats([
    [american,[burger,wings,steak,beef,chicken,american]],
    [chinese, [chicken,dumplings,noodles,rice,beef,chinese]],
    [japanese,[sushi,seafood,rice,japanese]],
    [italian, [pizza,pasta,italian]],
    [mexican, [tacos,burritos,rice,mexican]]
]).

nouns_list([
    burger,wings,steak,beef,chicken,dumplings,
    noodles,rice,sushi,seafood,pizza,pasta,tacos,
    burritos,spicy]).

price_list([
    cheap,expensive,average
    ]).

mode_list([
    fastfood,sitdown,delivery]).

categories_list([
    american,japanese,chinese,italian,mexican]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%Pre extablished KB%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%
knowledge_base([
    %%%%%%%%%%%%%%%%%%%%
    %%%% Categories %%%%
    %%%%%%%%%%%%%%%%%%%%

    %%%%American food%%%%
    is(buffalowildwings,american),is(applebees,american),is(mcdonalds,american),is(rubytuesdays,american),is(chickfila,american),is(kfc,american),is(cheesecakefactory,american),is(texasroadhouse,american),
    %%%%%% Chinese %%%%%%
    is(pfchangs,chinese),is(pandaexpress,chinese),is(chowcity,chinese),is(kq,chinese),
    %%%%% Japanese  %%%%%
    is(koto,japanese),is(kiyomi,japanese),is(snakebomb,japanese),is(oceansushi,japanese),is(ichiro,japanese),
    %%%%%  Italian  %%%%%
    is(olivegarden,italian),is(carrabbas,italian),is(bucadibeppo,italian),is(pizzahut,italian),is(dominos,italian),
    %%%%% Mexican %%%%%%
    is(tacobell,mexican),is(chipotle,mexican),is(fajitagrill,mexican),is(aztecha,mexican),is(laparrila,mexican),

    %%%%%%%%%%%%%%%%%%%%
    %%%%%  PRICE  %%%%%%
    %%%%%%%%%%%%%%%%%%%%

    %%%% Expensive %%%%%
    is(texasroadhouse,expensive),is(cheesecakefactory,expensive),is(pfchangs,expensive), is(snakebomb,expensive),
    %%% Normal Price %%%
    is(buffalowildwings,normalprice),is(applebees,normalprice),is(chickfila,normalprice),is(pandaexpress,normalprice),is(kq,normalprice),is(kiyomi,normalprice),is(koto,normalprice),is(olivegarden,normalprice),is(chipotle,normalprice),is(carrabbas,normalprice),is(fajitagrill,normalprice),is(rubytuesdays,normalprice),is(chowcity,normalprice)
    %%%%%% Cheap %%%%%%%
    is(mcdonalds,cheap),is(kfc,cheap),

    %%%%%%%%%%%%%%%%%%%
    %%%%   Mode   %%%%%
    %%%%%%%%%%%%%%%%%%%
    

    
    %%%%%%%%%%%%%%%%%%%%
    %%%%% KeyWords %%%%%
    %%%%%%%%%%%%%%%%%%%%

    has(buffalowildwings,wings),has(applebees,burger),has(mcdonalds,burger),has(rubytuesdays,steak),has(chickfila,chicken),has(kfc,chicken),has(texasroadhouse,steak),has(cheesecakefactory,beef),

    has(pfchangs,dumplings),has(pandaexpress,noodles),has(chowcity,rice),has(pfchangs,rice),has(chowcity,noodles),has(kq,beef),has(kq,chicken),has(kq,noodles),has(kq,noodles),

    has(koto,rice),has(kiyomi,sushi),has(kiyomi,seafood),has(snakebomb,rice),has(snakebomb,sushi),has(snakebomb,seafood),has(oceansushi,sushi),has(ichiro,rice),

    has(olivegarden,pasta),has(carrabbas,pasta),has(bucadibeppo,pasta),has(pizzahut,pizza),has(dominos,pizza),

    has(tacobell,tacos),has(tacobell,burritos),has(chipotle,rice),has(chipotle,burritos),has(fajitagrill,burritos),has(aztecha,burritos),has(laparrila,rice)
    
    ]).
