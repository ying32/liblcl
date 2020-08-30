
* [中文](README.md)   
* English   

----
# liblcl

A common cross-platform GUI library, the core uses Lazarus LCL.

----

[Compilation guide](Compile.README.en-US.md)  

----

* Languages supported:  

  * go: https://github.com/ying32/govcl  

  * c/c++: [Tools/genBind/c](Tools/genBind/c)    

* Highly completed languages:  

  * nim(Beta): https://github.com/ying32/nim-vcl  
  * rust(Test): https://github.com/ying32/rust-vcl  

* Language under test  




----

Language binding tool generator: [genBind](Tools/genBind), c/c++, rust and nim have been generated. Refer to the binding template files of these languages for details.

----

#### others  

*All exported functions are in the standard c way.*  Use the `__stdcall` convention on Windows, and the `__cdecl` convention on other platforms.

----

##### Character Encoding   

The `utf-8` encoding is used by default on all platforms.

----

##### Use structured exception handling functions   

*Note: If the `UsehandleException` compilation instruction is enabled in the `ExtDecl.inc` file of the liblcl source code, then there is no longer a need for `MySyscall` to handle exceptions, but the compiled file will increase, and it will increase by about 1M under Windows, Linux It will increase about 3M under macOS, and about 2.5M under macOS.*  

```c

// Type definition
typedef uint64_t(LCLAPI *MYSYSCALL)(void*, intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  

// Get this function from DLL
pMySyscall = (MYSYSCALL)get_proc_addr("MySyscall");  

// ----------- Instructions -----------  
#define DEFINE_FUNC_PTR(name) \
static void* p##name; 

#define GET_FUNC_ADDR(name) \
if(!p##name) \
   p##name = get_proc_addr(""#name""); \
assert(p##name != NULL); 

#define COV_PARAM(name) \
(uintptr_t)name

#define MySyscall(addr, len, a1, a2 , a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) \
    pMySyscall((void*)addr, (intptr_t)len, COV_PARAM(a1), COV_PARAM(a2), COV_PARAM(a3), COV_PARAM(a4), COV_PARAM(a5), COV_PARAM(a6), COV_PARAM(a7), COV_PARAM(a8), COV_PARAM(a9), COV_PARAM(a10), COV_PARAM(a11), COV_PARAM(a12))


// The function defined in this way can catch the exception thrown by LCL in DLL
DEFINE_FUNC_PTR(Application_Instance) 
TApplication Application_Instance() {
    GET_FUNC_ADDR(Application_Instance)
    return (TApplication)MySyscall(pApplication_Instance, 0 ,0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0);
}
```

----

##### Default instanced class

*No need to manually call create and release.*  

```c

// definition
TApplication Application; // Application
TScreen Screen;           // Screen
TMouse  Mouse;            // Mouse
TClipboard  Clipboard;    // Clipboard
TPrinter Printer;         // Printer  

// Get instance class pointer
Application = Application_Instance();
Screen = Screen_Instance();
Mouse = Mouse_Instance();              
Clipboard = Clipboard_Instance();      
Printer = Printer_Instance();          
```

----

##### Event callback

*Event callbacks are divided into 3 types.*

```c
// x86: sizeof(uintptr_t) = 4
// x64: sizeof(uintptr_t) = 8

// Get the parameters in the event from the specified index and address
#define getParamOf(index, ptr) \
 (*((uintptr_t*)((uintptr_t)ptr + (uintptr_t)index*sizeof(uintptr_t))))
```


* Basic event callback  

```c
 typedef void(*SYSCALL0)();  
 typedef void(*SYSCALL1)(intptr_t);  
 typedef void(*SYSCALL2)(intptr_t, uintptr_t);  
 typedef void(*SYSCALL3)(intptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL4)(intptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL5)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL6)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL7)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL8)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL9)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL10)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL11)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 typedef void(*SYSCALL12)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
 

// Callback function prototype
// f:        Id or function pointer passed in through SetOnXXX 
// args:     Parameter array pointer, Get each member by getParamOf
// argcount: Parameter array length
static void* LCLAPI doEventCallbackProc(void* f, void* args, long argCount) {
 
	#define _A_(index) \
	   getParamOf(index, args)

    switch (argCount) {
    case 0:  ((SYSCALL0) (f))(); break;
    case 1:  ((SYSCALL1) (f))(_A_(0)); break;
    case 2:  ((SYSCALL2) (f))(_A_(0), _A_(1)); break;
    case 3:  ((SYSCALL3) (f))(_A_(0), _A_(1), _A_(2)); break;
    case 4:  ((SYSCALL4) (f))(_A_(0), _A_(1), _A_(2), _A_(2)); break;
    case 5:  ((SYSCALL5) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4)); break;
    case 6:  ((SYSCALL6) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5)); break;
    case 7:  ((SYSCALL7) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6)); break;
    case 8:  ((SYSCALL8) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7)); break;
    case 9:  ((SYSCALL9) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8)); break;
    case 10: ((SYSCALL10)(f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8), _A_(9)); break;
    case 11: ((SYSCALL11)(f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8), _A_(9), _A_(10)); break;
    case 12: ((SYSCALL12)(f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8), _A_(9), _A_(10), _A_(11)); break;
    }
    // Always return NULL
    return NULL;
}

// Set callback
SetEventCallback(GET_CALLBACK(doEventCallbackProc));
```

* TForm message callback   
```c
// f: addr
// msg: TMessage
void* LCLAPI doMessageCallbackProc(void* f, void* msg) {
   ((void(*)(void*))f)(msg);
    return NULL;
}

// Set callback
SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
```

* Thread synchronization callback  
```c

static TThreadProc threadSyncProc;

void* LCLAPI doThreadSyncCallbackProc() {
    if (threadSyncProc) {
        ((TThreadProc)threadSyncProc)();
        threadSyncProc = NULL;
    }
    return NULL;
}

// Set callback
SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));

// Thread synchronization operation
void ThreadSync(TThreadProc fn) {
   
#ifdef __GNUC__
    pthread_mutex_lock(&threadSyncMutex);
#else
    EnterCriticalSection(&threadSyncMutex);
#endif
    threadSyncProc = fn;
    Synchronize(FALSE);
    threadSyncProc = NULL;
#ifdef __GNUC__
    pthread_mutex_unlock(&threadSyncMutex);
#else
    LeaveCriticalSection(&threadSyncMutex);
#endif
   
}
```

##### "set" type operation  

```c

// Lazarus "set" addition, an index stored as a bit in val... with a subscript of 0
TSet Include(TSet s, uint8_t val) {
    return (TSet)(s | (1 << val));
}

// "set" subtraction, index stored as a bit in val..., subscript 0
TSet Exclude(TSet s, uint8_t val) {
    return (TSet)(s & (~(1 << val)));
}

// Judgment of "set" type, val indicates the number of digits, and the subscript is 0
BOOL InSet(uint32_t s, uint8_t val) {
    if ((s&(1 << val)) != 0) {
        return TRUE;
    }
    return FALSE;
}
```

----

##### Initial liblcl example

```c
#define GET_CALLBACK(name) \
  (void*)&name
 
static void init_lib_lcl() {
#ifdef __GNUC__
    pthread_mutex_init(&threadSyncMutex, NULL);
#else
    InitializeCriticalSection(&threadSyncMutex);
#endif

    // Set the callback function of the event
    SetEventCallback(GET_CALLBACK(doEventCallbackProc));
    // Set message callback
    SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
    // Set thread synchronization callback
    SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));
    // Initial instance class
    Application = Application_Instance();
    Screen = Screen_Instance();
    Mouse = Mouse_Instance();            
    Clipboard = Clipboard_Instance();    
    Printer = Printer_Instance();        
}

static void un_init_lib_lcl() {
#ifdef __GNUC__
    pthread_mutex_destroy(&threadSyncMutex);
#else
    DeleteCriticalSection(&threadSyncMutex);
#endif
}
```

----

### C language call liblcl example  

```c
// main.c : 此文件包含 "main" 函数。程序执行将在此处开始并结束。
//

#include "liblcl.h" 

 
#ifdef _WIN32

char *UTF8Decode(char* str) {
    int len = MultiByteToWideChar(CP_UTF8, 0, str, -1, 0, 0);
    wchar_t* wCharBuffer = (wchar_t*)malloc(len * sizeof(wchar_t) + 1);
    MultiByteToWideChar(CP_UTF8, 0, str, -1, wCharBuffer, len);

    len = WideCharToMultiByte(CP_ACP, 0, wCharBuffer, -1, 0, 0, 0, NULL);
    char* aCharBuffer = (char*)malloc(len * sizeof(char) + 1);
    WideCharToMultiByte(CP_ACP, 0, wCharBuffer, -1, aCharBuffer, len, 0, NULL);
    free((void*)wCharBuffer);

    return aCharBuffer;
}
#endif

void onButton1Click(TObject sender) {
    ShowMessage("Hello world!");
}

void onOnDropFiles(TObject sender, void* aFileNames, intptr_t len) {
    printf("aFileNames: %p, len=%d\n", aFileNames, len);
    intptr_t i;
    for (i = 0; i < len; i++) {
        
#ifdef _WIN32
        char *filename = UTF8Decode(GetFPStringArrayMember(aFileNames, i));
#else
        char *filename = GetFPStringArrayMember(aFileNames, i);
#endif
        printf("file[%d]=%s\n", i+1, filename);
#ifdef _WIN32
        free((void*)filename);
#endif
    }
}

void onFormKeyDown(TObject sender, Char* key, TShiftState shift) {
    printf("key=%d, shift=%d\n", *key, shift);
    if (*key == vkReturn) {
        ShowMessage("press Enter!");
    }

    TShiftState s = Include(0, ssAlt);
    if (InSet(s, ssAlt)) {
        printf("ssAlt1\n");
    }
    s = Exclude(s, ssAlt);
    if (!InSet(s, ssAlt)) {
        printf("ssAlt2\n");
    }
}

void onEditChange(TObject sender) {
    printf("%s\n", Edit_GetText(sender));
}

int main()
{
#ifdef _WIN32
    if (load_liblcl("liblcl.dll")) {
#endif
#ifdef __linux__
    if (load_liblcl("liblcl.so")) {
#endif
#ifdef __APPLE__
    if (load_liblcl("liblcl.dylib")) {
#endif
        Application_SetMainFormOnTaskBar(Application, TRUE); 
        Application_SetTitle(Application, "Hello LCL"); 
        Application_Initialize(Application);
		
        TForm form = Application_CreateForm(Application, FALSE);
        Form_SetCaption(form, "LCL Form");
        Form_SetPosition(form, poScreenCenter);
        Form_SetAllowDropFiles(form, TRUE)
        Form_SetOnDropFiles(form, onOnDropFiles);
        Form_SetKeyPreview(form, TRUE);
        Form_SetOnKeyDown(form, onFormKeyDown);
        

        // 
        //TMemoryStream mem = NewMemoryStream();
        //MemoryStream_Write(mem, data, datalen);
        //MemoryStream_SetPosition(mem, 0); 
        //ResFormLoadFromStream(mem, form);
        //MemoryStream_Free(mem);
        
        // 
        //ResFormLoadFromFile("./Form1.gfm", form);

        TButton btn = Button_Create(form);
        Button_SetParent(btn, form);
        Button_SetOnClick(btn, onButton1Click);
        Button_SetCaption(btn, "button1");
        Button_SetLeft(btn, 100);
        Button_SetTop(btn, 100);
        
        TEdit edit = Edit_Create(form);
        Edit_SetParent(edit, form);
        Edit_SetLeft(edit, 10);
        Edit_SetTop(edit, 10);
        Edit_SetOnChange(edit, onEditChange);

        Application_Run(Application);

        close_liblcl();
    }
    return 0;
}
```
----

### LICENSE  

**Keep the same license agreement with Lazarus LCL components: [COPYING.modifiedLGPL](COPYING.modifiedLGPL.txt)**