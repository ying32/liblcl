
* [中文](README.md)   
* English   

----
**Check out the [dev](https://github.com/ying32/liblcl/tree/dev) branch for new features.**
----

# liblcl

A common cross-platform GUI library, the core uses Lazarus LCL.

----

[Compilation guide](Compile.README.en-US.md)  

----

* Languages supported:  

  * go: https://github.com/ying32/govcl  

  * c/c++: [Tools/genBind/c](Tools/genBind/c)    

----

Language binding tool generator: [genBind](Tools/genBind), c/c++, rust and nim have been generated. Refer to the binding template files of these languages for details.

----

#### others  

*All exported functions are in the standard c way.*  Use the `__stdcall` convention on Windows, and the `__cdecl` convention on other platforms.

----

##### Character Encoding   

The `utf-8` encoding is used by default on all platforms.

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
typedef void(*ESYSCALL0)();  
typedef void(*ESYSCALL1)(intptr_t);  
typedef void(*ESYSCALL2)(intptr_t, uintptr_t);  
typedef void(*ESYSCALL3)(intptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL4)(intptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL5)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL6)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL7)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL8)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL9)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL10)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL11)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  
typedef void(*ESYSCALL12)(intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t); 
 

// Callback function prototype
// f:        Id or function pointer passed in through SetOnXXX 
// args:     Parameter array pointer, Get each member by getParamOf
// argcount: Parameter array length
static void* LCLAPI doEventCallbackProc(void* f, void* args, long argCount) {
 
	#define _A_(index) \
	   getParamOf(index, args)

    switch (argCount) {
    case 0:  ((ESYSCALL0) (f))(); break;
    case 1:  ((ESYSCALL1) (f))(_A_(0)); break;
    case 2:  ((ESYSCALL2) (f))(_A_(0), _A_(1)); break;
    case 3:  ((ESYSCALL3) (f))(_A_(0), _A_(1), _A_(2)); break;
    case 4:  ((ESYSCALL4) (f))(_A_(0), _A_(1), _A_(2), _A_(2)); break;
    case 5:  ((ESYSCALL5) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4)); break;
    case 6:  ((ESYSCALL6) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5)); break;
    case 7:  ((ESYSCALL7) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6)); break;
    case 8:  ((ESYSCALL8) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7)); break;
    case 9:  ((ESYSCALL9) (f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8)); break;
    case 10: ((ESYSCALL10)(f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8), _A_(9)); break;
    case 11: ((ESYSCALL11)(f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8), _A_(9), _A_(10)); break;
    case 12: ((ESYSCALL12)(f))(_A_(0), _A_(1), _A_(2), _A_(3), _A_(4), _A_(5), _A_(6), _A_(7), _A_(8), _A_(9), _A_(10), _A_(11)); break;
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