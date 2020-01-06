# Class that group several related functions.
class Calc:
    def __init__(self,a,b): #self as a variable whose 
                            #sole job is to learn the name 
                            #of a particular instance, below this name is x
        self.a = a
        self.b = b
    def add(self): #note that Calc will have a,b parameters, 
                   #the add method will not have entry parameters
        return self.a+self.b
    def sub(self):
        return self.a-self.b
    def mult(self):
        return self.a*self.b
    def div(self):
        return self.a/self.b