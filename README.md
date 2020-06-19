# liblcl
一个通用的跨平台GUI库，核心使用Lazarus LCL。

A common cross-platform GUI library, the core uses Lazarus LCL.

----

源代码(Source code)：https://github.com/ying32/govcl/tree/master/UILibSources  

___

已支持语言(Languages supported)：

go: https://github.com/ying32/govcl  

c/c++: https://github.com/ying32/govcl/tree/master/Tools/makeCHeader/test  

----

#### 其他(others)   

*所有导出的函数都为标准的c方式。* 在Windows上采用`__stdcall`约定，其它平台采用`__cdecl`约定。

*All exported functions are in the standard c way.*  Use the `__stdcall` convention on Windows, and the `__cdecl` convention on other platforms.

----

##### 字符编码(Character Encoding)   

在所有平台上都默认使用`utf-8`编码。

The `utf-8` encoding is used by default on all platforms.

----

##### 使用结构化异常处理的函数  

##### Use structured exception handling functions   

*注： 如果在liblcl源代码`ExtDecl.inc`文件中启用了`UsehandleException`编译指令，则不再需要`MySyscall`处理异常，但编译出的文件会增大，Windows下会增加约1M左右，Linux下会增加3M左右，macOS下会增加2.5M左右。*  

*Note: If the `UsehandleException` compilation instruction is enabled in the `ExtDecl.inc` file of the liblcl source code, then there is no longer a need for `MySyscall` to handle exceptions, but the compiled file will increase, and it will increase by about 1M under Windows, Linux It will increase about 3M under macOS, and about 2.5M under macOS.*  

```c
// 类型定义
// Type definition
typedef uint64_t(LCLAPI *MYSYSCALL)(void*, intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  

// 从DLL中获取此函数
// Get this function from DLL
pMySyscall = (MYSYSCALL)get_proc_addr("MySyscall");  

// ----------- 使用方法(Instructions) -----------  
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


// 如此定义的函数就可以捕捉DLL中LCL抛出的异常
// The function defined in this way can catch the exception thrown by LCL in DLL
DEFINE_FUNC_PTR(Application_Instance) 
TApplication Application_Instance() {
    GET_FUNC_ADDR(Application_Instance)
    return (TApplication)MySyscall(pApplication_Instance, 0 ,0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0);
}
```

----

##### 默认的实例类(Default instanced class)

*无需手动调用创建和释放。*  

*No need to manually call create and release.*  

```c
// 定义
// definition
TApplication Application; // 应用程序(Application)
TScreen Screen;           // 屏幕(Screen)
TMouse  Mouse;            // 鼠标(Mouse)
TClipboard  Clipboard;    // 剪切板(Clipboard)
TPrinter Printer;         // 打印机(Printer)  

// 获取实例类指针
// Get instance class pointer
Application = Application_Instance();
Screen = Screen_Instance();
Mouse = Mouse_Instance();              
Clipboard = Clipboard_Instance();      
Printer = Printer_Instance();          
```

----

##### 事件回调(Event callback)

*事件回调分为3种类型。*   

*Event callbacks are divided into 3 types.*

获取参数数组中每个成员  

```c
// x86: sizeof(uintptr_t) = 4
// x64: sizeof(uintptr_t) = 8

// 从指定索引和地址获取事件中的参数
// Get the parameters in the event from the specified index and address
#define getParamOf(index, ptr) \
 (*((uintptr_t*)((uintptr_t)ptr + (uintptr_t)index*sizeof(uintptr_t))))
```



* 基本事件回调(Basic event callback)  

```c
#define GET_VAL(index) \
getParamOf(index, args)

#define Syscall12(addr, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) \
    ((void(*)(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))addr)(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)


// 回调函数原型
// Callback function prototype
// f:        通过SetOnXXX传入的Id或者函数指针
//           Id or function pointer passed in through SetOnXXX 
// args:     参数组数指针，通过getParamOf来获取每个成员
//           Parameter group number pointer, Get each member by getParamOf
// argcount: 参数数组长度
//           Parameter array length
void* LCLAPI doEventCallbackProc(void* f, void* args, long argcount) {
      switch (argcount) {
    case 0: Syscall12(f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
       break;
    
    case 1: Syscall12(f, GET_VAL(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
       break;
    
    case 2: Syscall12(f, GET_VAL(0), GET_VAL(1), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
        break;
    
    case 3: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), 0, 0, 0, 0, 0, 0, 0, 0, 0);
       break;
    
    case 4: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(2), 0, 0, 0, 0, 0, 0, 0, 0);
       break;
    
    case 5: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), 0, 0, 0, 0, 0, 0, 0);
       break;
    
    case 6: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), 0, 0, 0, 0, 0, 0);
       break;
    
    case 7: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), GET_VAL(6), 0, 0, 0, 0, 0);
       break;
    
    case 8: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), GET_VAL(6), GET_VAL(7), 0, 0, 0, 0);
       break;
    
    case 9: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), GET_VAL(6), GET_VAL(7), GET_VAL(8), 0, 0, 0);
       break;
    
    case 10: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), GET_VAL(6), GET_VAL(7), GET_VAL(8), GET_VAL(9), 0, 0);
       break;
    
    case 11: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), GET_VAL(6), GET_VAL(7), GET_VAL(8), GET_VAL(9), GET_VAL(10), 0);
       break;
    
    case 12: Syscall12(f, GET_VAL(0), GET_VAL(1), GET_VAL(2), GET_VAL(3), GET_VAL(4), GET_VAL(5), GET_VAL(6), GET_VAL(7), GET_VAL(8), GET_VAL(9), GET_VAL(10), GET_VAL(11));
       break;
    }
    // 总是返回NULL即可
    // Always return NULL
    return NULL; 
}
// 设置回调
// Set callback
SetEventCallback(GET_CALLBACK(doEventCallbackProc));
```

* TForm消息回调TForm message callback() 
```c
// f: addr
// msg: TMessage
void* LCLAPI doMessageCallbackProc(void* f, void* msg) {
   ((void(*)(void*))f)(msg);
    return NULL;
}

// 设置回调
// Set callback
SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
```

* 线程同步回调(Thread synchronization callback)  
```c

static TThreadProc threadSyncProc;

void* LCLAPI doThreadSyncCallbackProc() {
    if (threadSyncProc) {
        ((TThreadProc)threadSyncProc)();
        threadSyncProc = NULL;
    }
    return NULL;
}

// 设置回调
// Set callback
SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));

// 线程同步操作
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

##### 集合类型操作("set" type operation)  

```c
// 集合加法，val...中存储为位的索引，下标为0
// Lazarus "set" addition, an index stored as a bit in val... with a subscript of 0
TSet Include(TSet s, uint8_t val) {
    return (TSet)(s | (1 << val));
}

// 集合减法，val...中存储为位的索引，下标为0
// "set" subtraction, index stored as a bit in val..., subscript 0
TSet Exclude(TSet s, uint8_t val) {
    return (TSet)(s & (~(1 << val)));
}

// 集合类型的判断，val表示位数，下标为0
// Judgment of "set" type, val indicates the number of digits, and the subscript is 0
BOOL InSet(uint32_t s, uint8_t val) {
    if ((s&(1 << val)) != 0) {
        return TRUE;
    }
    return FALSE;
}
```

----

##### 初始liblcl示例(Initial liblcl example)

```c
#define GET_CALLBACK(name) \
  (void*)&name
 
static void init_lib_lcl() {
#ifdef __GNUC__
    pthread_mutex_init(&threadSyncMutex, NULL);
#else
    InitializeCriticalSection(&threadSyncMutex);
#endif

    // 设置事件的回调函数
    // Set the callback function of the event
	SetEventCallback(GET_CALLBACK(doEventCallbackProc));
	// 设置消息回调
    // Set message callback
	SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
	// 设置线程同步回调
    // Set thread synchronization callback
	SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));
    // 初始实例类
    // Initial instance class
	Application = Application_Instance();
	Screen = Screen_Instance();
	Mouse = Mouse_Instance();            
	Clipboard = Clipboard_Instance();    
	Printer = Printer_Instance();        

#ifdef _WIN32
    // 尝试加载exe中名为MAINICON的图标为应用程序图标
    // Try to load the icon named MAINICON in the exe as the application icon
    if(Application) {
        TIcon icon = Application_GetIcon(Application);
        if(icon) {
            Icon_SetHandle(icon, LoadIconA(GetModuleHandleA(NULL), "MAINICON"));
        } 
    }
#endif
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

### c语言调用liblcl示例(C language call liblcl example)  

```c
// main.c : 此文件包含 "main" 函数。程序执行将在此处开始并结束。
//

#include "liblcl.h" 

 
#ifdef _WIN32
// UTF8解码
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

// 按钮单击事件
void onButton1Click(TObject sender) {
    ShowMessage("Hello world!");
}

// 文件拖放事件
void onOnDropFiles(TObject sender, void* aFileNames, intptr_t len) {
    printf("aFileNames: %p, len=%d\n", aFileNames, len);
    intptr_t i;
    // GetStringArrOf 为一个从Lazarus的string数组中获取成员的函数。
    for (i = 0; i < len; i++) {
        
#ifdef _WIN32
        // 由于liblcl使用的是UTF-8编码，所以获取或者传入的在Windows下都要经过UTF-8编/解码 
        char *filename = UTF8Decode(GetStringArrOf(aFileNames, i));
#else
        // Linux与macOS默认都是UTF-8，则无需编/解码
        char *filename = GetStringArrOf(aFileNames, i);
#endif
        printf("file[%d]=%s\n", i+1, filename);
#ifdef _WIN32
        free((void*)filename);
#endif
    }
}

// 窗口键盘按下事件
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

// 编辑框内容改变事件
void onEditChange(TObject sender) {
    printf("%s\n", Edit_GetText(sender));
}

int main()
{
    // 加载库
#ifdef _WIN32
    if (load_liblcl("liblcl.dll")) {
#endif
#ifdef __linux__
    if (load_liblcl("liblcl.so")) {
#endif
#ifdef __APPLE__
    if (load_liblcl("liblcl.dylib")) {
#endif

        // 主窗口显示在任务栏，仅Windows有效
        Application_SetMainFormOnTaskBar(Application, TRUE); 
        // 应用程序标题，影响到：比如ShowMessage的标题。
        Application_SetTitle(Application, "Hello LCL"); 
        // 初始化应用程序
        Application_Initialize(Application);

        // 创建窗口
        TForm form = Application_CreateForm(Application, FALSE);
        // 设置窗口标题
        Form_SetCaption(form, "LCL Form");
        // 设置窗口位置
        Form_SetPosition(form, poScreenCenter);

        // --- 拖放文件测试 ---
        // 接受文件拖放
        Form_SetAllowDropFiles(form, TRUE); 
        // 拖放文件事件
        Form_SetOnDropFiles(form, onOnDropFiles);

        // 窗口优先接受按键，不受其它影响
        Form_SetKeyPreview(form, TRUE);

        // 窗口按键事件
        Form_SetOnKeyDown(form, onFormKeyDown);
        
        // ---------- 从内存流或者文件加载UI布局文件 ----------
        // 从文件加载窗口设置
        // 从流加载
        //TMemoryStream mem = NewMemoryStream();
        //MemoryStream_Write(mem, data, datalen);
        //MemoryStream_SetPosition(mem, 0); 
        //ResFormLoadFromStream(mem, form);
        //MemoryStream_Free(mem);
        
        // 从文件加载
        //ResFormLoadFromFile("./Form1.gfm", form);

        // ----------  动态创建控件 ---------- 
        // 创建一个按钮
        TButton btn = Button_Create(form);
        // 设置子父窗口
        Button_SetParent(btn, form);
        // 设置按钮单击事件
        Button_SetOnClick(btn, onButton1Click);
        // 设置按钮标题
        Button_SetCaption(btn, "button1");
        // 设置按钮在Parent的左边位置
        Button_SetLeft(btn, 100);
        // 设置按钮在Parent的顶边位置
        Button_SetTop(btn, 100);
        
        // 创建一个单行文件框（多行为TMemo）
        TEdit edit = Edit_Create(form);
        // 设置子父窗口
        Edit_SetParent(edit, form);
        // 设置左边
        Edit_SetLeft(edit, 10);
        // 设置顶边
        Edit_SetTop(edit, 10);
        // 设置编辑器内容改变事件
        Edit_SetOnChange(edit, onEditChange);

        // 运行app
        Application_Run(Application);

        // 释放liblcl库
        close_liblcl();
    }
    return 0;
}
```

