#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int token;
char *src = NULL;
char *old_src;

char *data; //Data Segment for memory
int *text; //Text Segment for memory
int *stack; //Stack Segment for memory
int *idmain; 


int poolsize;
int *pc; //program counter
int *bp , *sp , ax , cycle; //virtual machine registers

int line;
int *symbol;
int *current_id;
int token_val;

//Instructions
enum{LEA, IMM, JMP, CALL, JZ, JNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PUSH,
     OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
     OPEN, READ, CLOS, PRTF, MALC, MSET, MCMP, EXIT};

//tokens
enum{Num = 128, Fun, Sys, Glo, Loc, Id, Char, Else, Enum, If, Int, Return, Sizeof, While,
      Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div,
	Mod, Inc, Dec, Brak};      

//fields of identifier
enum{Token, Hash, Name, Type, Class, Value, BType, BClass, BValue, Idsize};
 
//type of ptr
enum{CHAR, INT, PTR};

//type of declaration
enum{Global, Local};




//lexer , parser implementation
void next(){
    char *last_pos;
    int hash;
    
    while(*src == ' ' || *src == '\t'){
        src++;
    }

    while(token = *src){
        ++src;
	if(token == '\n'){
	    ++line;
	}
	else if(token == '#'){
	    //skip macro and include
	    while(*src != 0 && *src != '\n'){
	        ++src;
	    }
	}
	else if((token >= 'a' && token <= 'z') || (token >= 'A' && token <= 'Z') || (token == '_')){
	    last_pos = src - 1;
	    hash = token;

	    while((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_')){
	        hash = hash * 147 + *src;
		src++;
	    }
            
	    current_id = symbol;
	    //linear time complexity approach to find symbol in symbol table
	    while(current_id){
	        if(current_id[Hash] == hash && !memcmp((char*)current_id[Name], last_pos, src - last_pos)){
		    //found symbol and return
		    token = current_id[Token];
		    return;
		}
		current_id = current_id + Idsize;
	    }
	    
     	    current_id[Name] = (int)last_pos;
	    current_id[Hash] = hash;
	    token = current_id[Token] = Id;
	    return;	    
	}
	else if(token >= '0' && token <= '9'){
	    token_val = token - '0';
	    if(token_val){
	        if(*src == 'x' || *src == 'X'){
		    //hexidecimal
		    token = *++src;
		    while((token >= '0' && token <= '9') || (token >= 'a' && token <= 'f') || (token >= 'A' && token <= 'F')){
		        token_val = token_val * 16 + (token & 15) + (token >= 'A' ? 9 : 0);
			token = *++src;
		    }
		}else{
		    //decimal
		    while(*src >= '0' && *src <= '9'){
		        token_val = token_val * 10 + (*src - '0');
		    }
		}
	    }else{
                    //oct
                    while(*src >= '0' && *src <= '7'){
                        token_val = token_val * 8 + (*src - '0');
                    } 
            }
	    token = Num;
	    return;
	}
	else if(token == '/'){
	     //it can be comment or divide token
	     if(*src == '/'){
	        while(*src != 0 && *src != '\n'){
		    ++src;
		}
	    }else{
	        token = Div;
		return;
	    }
	}
	else if(token == '"' || token == '\''){
	    last_pos = data;
	    while(*src != 0 && *src != token){
		token_val = *src++;
		if(token_val == '\\'){
		    token_val = *src++;
		    if(token_val == 'n'){
		    	token_val = '\n';
		    }
		}
		if(token_val == '"'){
		    *data++ = token_val;
		}		
	    }

	    src++;
	    if(token == '"'){
	        token_val = (int)last_pos;
	    }else{
	    	token = Num;
	    }
	    return;
	}
	else if(token == '='){
	    if(*src == '='){
	        src++;
	        token = Eq;
	    }else{
	        token = Assign;
	    }
	    return;
	}
	else if(token == '+'){
	    if(*src == '+'){
	        src++;
		token = Inc;
	    }else{
	    	token = Add;
	    }
	    return;
	}
	else if(token == '-'){
	    if(*src == '-'){
	        src++;
		token = Dec;
	    }else{
	    	token = Sub;
	    }
	    return;
	}
	else if(token == '*'){
	    token = Mul;
	    return;
	}
	else if(token == '!'){
	    if(*src == '='){
	        token = Ne;
		src++;
	    }
	    return;
	}
	else if(token == '<'){
	    if(*src == '='){
	        token = Le;
		src++;
	    }else if(*src == '<'){
	        token = Shl;
		src++;
	    }else{
	        token = Lt;
	    }
	    return;
	}
	else if(token == '>'){
	    if(*src == '='){
	        token = Ge;
		src++;
	    }else if(*src == '>'){
	        token = Shr;
		src++;
	    }else{
	    	token = Gt;
	    }
	    return;
	}
	else if(token == '|'){
	    if(*src == '|'){
	        token = Lor;
		src++;
	    }else{
	        token = Or;
	    }
	    return;
	}
	else if(token == '&'){
	    if(*src == '&'){
	        token = Lan;
		src++;
	    }else{
	        token = And;
	    }
	    return;
	}
	else if(token == '^'){
	    token = Xor;
	    return;
	}
	else if(token == '%'){
	    token = Mod;
	    return;
	}
	else if(token == '['){
	    token = Brak;
	    return;
	}
	else if(token == '?'){
	    token = Cond;
	    return;
	}
	else if (token == '~' || token == ';' || token == '{' || token == '}' || token == '(' || token == ')' || token == ']' || token == ',' || token == ':'){
	    return;
	}
    }
}

//TODO : implement expr()
int expr(){}

int factor(){
    int value = 0;
    if(token == '('){
        
    }
}

void program(){
    next();
    while(token){
        printf("%c" , token);
	next();
    }
    return;
}

int eval(){
    //TODO:implement evaluation of program
    return 0;
}
int main(int argc , char **argv){
    int fd , i;
    poolsize = 256*1024;
    argv++;
    char *s = *argv;
   // printf("%s\n" , s);
   
    fd = open(*argv , 0);
    printf("%d\n" , fd);
    if(fd < 0){
        printf("Fail to open file %s\n" , s);
	return -1;
    }
    
    if(!(text = malloc(poolsize))){
        printf("Error : could not malloc enough memory for text segment!\n");
	return -1;
    }
    
    if(!(data = malloc(poolsize))){
        printf("Error : could not malloc enough memory for data segment!\n");
	return -1;
    }

    if(!(stack = malloc(poolsize))){
        printf("Error : could not malloc enough memory for stack segment!\n");
	return -1;
    }

    if(!(symbol = malloc(poolsize))){
        printf("Error : could not malloc enough memory for symbol table!\n");
        return -1;
    }
    
    
    memset(text , 0 , poolsize);
    memset(data , 0 , poolsize);
    memset(stack , 0 , poolsize);
    memset(symbol , 0 , poolsize);

    //add reserved words to symbol table
    src = "if else char int enum return sizeof while";
    i = Char;
    while(i <= While){
        next();
	current_id[Token] = i++;
    }
    
    //add Library to symbol table
    src = "open read close printf malloc memset memcmp exit void main";
    i = Open;
    while(i <= EXIT){
        next();
	current_id[Class] = Sys;
	current_id[Type] = INT;
	current_id[Value] = i++;
    }

    next();
    current_id[Token] = Char; //handle void symbol

    next();
    idmain = current_id; //keep track of main

    if(!(src = old_src = malloc(poolsize))){
        printf("could not malloc (%d) for source area\n", poolsize);
	return -1;
    }
    
    if((i = read(fd , src , poolsize - 1)) <= 0){
        printf("read() return %d \n" , i);
	return -1;
    }

    src[i] = 0; //Add EOF character
    close(fd);

    program();

    if(!(pc = (int *)idmain[Value])){
        printf("Error : main() is not defined!\n");
    }
    

    return eval();
}
