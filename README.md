# Sgit
Scala git like program

Clone this repository and cd into the project directory.

How to run the program : 
The first option is to run `source installation.sh` into the project directory. 
You can now use any `sgit` command you would like.
 (If the rights are not granted you should do `chmod u+wx sgit.sh` the write rights are used to write your current path into sgit.sh).

> Manually:

You will need to perform this set of commands to install sgit.
* `chmod u+x sgit.sh`
* `ln -s sgit.sh sgit`
* `sbt assembly`
* ``export PATH=$PATH:`pwd` ``
> Then to write the path of the JAR file into sgit.sh you will need to set the location:
* `` echo "java -jar `pwd`/target/scala-2.13/SGit-assembly-0.1.jar $"*"" > sgit.sh ``

Now, you can run it from anywhere like so: custom_name sgit *ENTER* {args}

## Please be aware that you have to first call the sgit command press enter and THEN give your args
If for some reason you have an error like “Could Not Create the Java Virtual Machine” I recommend you to try the 2 methods, to fix this problem, described at : https://appuals.com/fix-could-not-create-the-java-virtual-machine/

Here are some explanation about the project : Input are dispatched toward several services, each of them has its own “main” function that orchester other functions and use the generic tools from the IO package (Input/Output) to store and display the changes. First the user will give some order that will be parsed by the Command_Parser object, this parser will check if the service asked is available and will dispatch the information to the right service provider (the services are each of the files represented at the bottom line of the schema). This service will get the context of the user (which repository he is in) and then work on his own, using some services from other Services providers in order to deliver the asked service.
