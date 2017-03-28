#ifndef __NAME_H__ // Where name is the name of the sk file
#define __NAME_H__

enum name_fsmName_state_t { /* STATES in this fsm */ }
enum name_fsmName2_state_t { /* etc for all fsms in the file */ } 
...

enum name_[enum_name]_enum_t { /* enum types user defined in sk file */ } 
...

#define name_fsmName_stateName1 0
#define name_fsmName_stateName2 1
...

#define name_typeName_typeValue 0
...


struct name_input { /* input definition */ 
        inputType variableName;
        ...
}
struct name_output { /* output definition */ 
        outputType variableName;
}

struct name_state { 
    int fsmName; /* Will hold current state of the fsm */
    ... /* do this for all fsm in file */

    /* ALL LOCAL VARAIBLES THAT EXISTS IN ALL FSM 
     * variable names: fsmName_variableName 
     */ 
    localVarType fsmName_variableName
}

int name_tick(struct name_state *, struct name_input *, struct name_output *);

#endif 
