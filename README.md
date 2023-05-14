# Project-for-DMM
Group 7 project

The project is made out of 5 tasks per total.
They are divided as followed:

Task 1: Task1_1() and Task1_2()
Task1_1: Reads the file in which the resources are located. Shows what type of generators there are and how many of them.
Task1_2: Lets the user add or substract a number of a certain resource. The number can not be below 0.

Task2: Task2()
Creates and writes a new file that contains the data about the resources. The user is asked for how many watts the resource generates and for how many hours per day. The user is asked as many times as there are resources ( if there are 1 of each kind => 3 then the user
is asked for 6 inputs per total ). It also calculates for daily, weekly and monthly energy that is generated by each of them.

Task 3: Task3_1() and Task3_2()
Task3_1: Writes the file that was created in Task2
Task3_2: Lets the user change a specific resource's data. For example, if one wants to change the data of a Solar panel they can rewrite a new production of watts and a new number of hours that it works for per day.

Task 4: Task4.data_analysis(Task4.caselist) and Task4.OrderAlgo(Task4.caselist)
Task4.data_analysis(Task4.caselist): Analyses the data from the file that was created in Task2. It can calculate the mean, median, mode, range and midrange based on hourly/daily/weekly/monthly values.
Task4.OrderAlgo(Task4.caselist): Sorts the Task2's file in ascendant order based on how much power a resource generates hourly/daily/weekly/monthly.

Task 5: Task5()
Checks if there are any malfunctions within the file. If the output is too low then it raises an error and tells the user where; If there is a malfuntion ( if one resource does not produce anything on a daily basis ) it means that the resource is not powered, thus creating an error
