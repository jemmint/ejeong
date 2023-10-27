#creating variables
animal = "cat"
name = "Vlad"
age = 4
is_adult = age >=3

'''
how to 
annotate
'''

print("my "+animal+" is "+name)
print(name,"is",age,"years old")
print(name+ "is an adult?" +str(is_adult))

station = "sadang"
print("The train going to",station,"is coming")

#make a calculator
num1 = input("Enter a number: ")
num2 = input("Enter another number: ")
result = int(num1) + int(num2)
print(result)

#random number

#what is the best programming language?
print("Python")

#operators
print(2*3) #multiply
print(2/3) #division
print(2**3) #2^3
print(5%3) #remainder
print(5//3) #quotient
print(10>3) #true
print(4 >=7) #false
print(4==4) #true
print(1 !=3)
print(not(1 !=3))
print((3>1) & (2==2)) #and
print((3>=4)| (2>3)) #or
print(abs(-5)) #5
print(pow(4,2)) #4^2
print(min(2,3,4)) #min, max, round

number = 2+3+4
number+=2
number*=3
number/=2
number-=2
number%=2
print(number)

from math import *
print(floor(4.99)) #round off

#random function
from random import*
print(random()) #print random value (>0.0 & <1.0)
print(int(random())) #integers only
print(randrange(1,46)) #select random number from 1 to 45



#using Math library
from math import *
print(floor(4.99)) #round off
print(ceil(3.14)) #round up
print(sqrt(16)) #square root



#strings & funtions
sentence = """
I am a boy,
you are a girl
"""
print(sentence)
print(sentence.lower())
print(sentence.upper())
print(sentence[0].isupper())
print(len(sentence))
print(sentence.replace("I","You"))
index = sentence.index("a") #printing position
print(sentence.find("a")) #printing position
print(sentence.count("a")) #number of times "a" appears in the sentence 

#making sentences with different formats  
print("I like %s and %s." % ("blue", "red")) # %s works for numbers and characthers
print("I like {0} and {1}.".format("blue", "red")) 
print("I like {color1} and {color2}.".format(color1="blue", color2="red")) 

print("I am a boy\n you are a girl") #change lines
print("I am a \"boy\".") #\", \': quotation marks, \\=\, \r: move cursor at the very front, \t: tab

#slicing
jumin = "123456-7891012"
print("gender:",jumin[7])
print("year:",jumin[:2]) #from 0 to 1 (2 is not inlcuded)

#quiz
url= "http://naver.com"
url= url.replace("http://","")
password = url[:3],len(url),url.count("e"),"!"
print("Your password in {site} is {password}.".format(site=url,password=password))

#list
subway = [1,2,3]
subway2 = subway.append(4)
subway.insert(1,1.5)
subway.pop() #remove the value at the very end
print(subway.count(1))
print(subway.sort()) #sorting in an ascending order
print(subway.reverse()) #reversing the order
subway.clear() #remove all values in the subway list
#subway.extend(subway2) #merging lists

#dictionary
cabinet = {1:"Ellie", 3:"Eunmi"} #assigning values
cabinet[1]="Yaxuan"
cabinet[4]="Zihan"
del cabinet[4]
print(cabinet.get(0)) #print a value on xth place
print(1 in cabinet) #true
print(cabinet.keys())
print(cabinet.values())
print(cabinet.items()) #print keys & values
#Tuple
(name,age,hobby) = ("Ellie", 35, "coding")
print(name,age,hobby)
#set (no duplicates, no order)
my_set = {1,2,3,3} #print 1,2,3 only 
my_set.add(10)
my_set.remove(10)
my_set2 = set[4,5,3]
#intersection
# print(my_set & my_set2)
# print(my_set.intersection(my_set2))
# #union
# print(my_set| my_set2)
# print(my_set.union(my_set2))
# #difference
# print(my_set-my_set2)
# print(my_set.difference(my_set2))

#chaning the structure of data
menu = {"milk", "coffee"} #{ }: set, [ ]: list, ( ): tuple
print(menu.type(menu))
menu = list(menu)

#quiz
from random import*
users = range(1,21) #type
users = list(users)
shuffle(users) #mix values randomly
winners = sample(users,4) #select 4 random users
print("announcement\n coffee: {0}\n icecream: {1}\n congrats!".format(winners[0],winners[1:]))

#if (conditional statement)
weather = input("How's weather today?")
if weather == "rain" or weather == "snow":
    print("bring your umbrella")
elif weather == "dust":
    print("wear a mask")
else: print("nothing to bring")

temp = int(input("What's the temperature today"))
if 30 <= temp:
    print("It's too hot")
elif 10 <= temp and temp <30:
    print("Good to go out")
else:
    print("It's too cold")

#for (repetitive statement)
customer = ["a", "b", "c"]
for waiting_no in customer:
    print("{0}, your drink is ready".format(waiting_no))
for waiting_no in range(0,5):
    print("waiting no: {0}".format(waiting_no))

student = [1,2,3]
student = [i+100 for i in student]
customer = ["iron man", "thor"]
customer = [len(i) for i in customer]   

#while (repetitive statement until condition is met)
customer = "a"
index = 5
while index >=1:
    print("{0}, coffee is ready. {1} remains.".format(customer,index))
    index -=1
    if index ==0:
        print("closed")

customer = "b"
person = "unknown"
while person !=customer:
    print("{0}, coffee is ready.".format(customer))
    person = input("What is your name?")
#indefinite loop (ctrl c: force to stop)
customer = "a"
index=1
while True:
    print("{0}, coffee is ready. call {1}.".format(customer, index))
    index +=1 


# #Continue (do not print & move to the next) & break (do not print & terminate conditional statement)
# absent = [2,5]
# no_book = [7]
# for student in range(1,11):
#     if student in absent:
#         continue
#     elif student in no_book:
#         break
#     print("{0}, read page {0}".format(student))    

#Quiz
from random import*
count = 0
for i in range(1,51):
    time = randrange(5,51)
if 5 <=time <=15:
    print("[o] {0}".format(i))
    count +=1
else: 
    print("[ ] {0}".format(i))    
print("Total number of passengers: {0}".format(count))

#Function
def open_account():
    print("New account has been opened")

def deposit(balance, money):
    print("Deposit has been completed. Your balance is {0}.".format(balance+money))
    return balance+money 

def withdraw(balance, money):
    if balance >= money:
        print("Get your money. Your balance is {0}.".format(balance-money))
        return balance-money
    else:
        print("Withdrawal failed. Your balance is {0}.".format(balance))
        return balance
def withdraw_night(balance, money):
    commission = 100
    return commission, balance-money-commission

balance = 0
balance = deposit(balance,1000)
commission, balance = withdraw_night(balance, 500)
print("commission is {0}, balance is {1}.".format(commission, balance))

#Basic value (should be defined at the very end)
def profile(name, age, main_lang):
    print("name: {0}\tage: {1}\tmain_lang:{2}.".format(name, age, main_lang))
profile("a", 20, "English")

def profile(name, age=17, main_lang=Python):
    print("name: {0}\tage: {1}\tmain_lang:{2}.".format(name, age, main_lang))
profile("a", 20, "English")

def profile(name, main_lang, age=17):
    print("name: {0}\tmain_lang: {1}\tage:{2}.".format(name, main_lang, age))
profile("a", 20, "English")

#get the value of the function
def profile(name, age, main_lang):
    print("name: {0}\tage: {1}\tmain_lang:{2}.".format(name, age, main_lang))   
def profile(name, age, main_lang):
    print(name, age, main_lang)
profile(name="a", main_lang="English", age=20)

#find TSLA stock price today 
import requests
from bs4 import BeautifulSoup
url = "https://finance.naver.com/item/main.nhn?code=005930"
res = requests.get(url)
res.raise_for_status()
soup = BeautifulSoup(res.text, "lxml")

#make a big red circle
import turtle as t
t.shape("turtle")
t.color("red")
t.circle(100)


