
* 中文   
* [English](README.en-US.md)   

----
**新功能请查看[dev](https://github.com/ying32/liblcl/tree/dev)分支。**
----

# liblcl
一个通用的跨平台GUI库，核心使用Lazarus LCL。

----

[编译指南](Compile.README.md)  

----

* 已支持语言： 

  * go: https://github.com/ying32/govcl  

  * c/c++: [Tools/genBind/c](Tools/genBind/c)    

----

语言绑定工具生成器：[genBind](Tools/genBind)，目前已经生成了c/c++、rust、nim的，详细参考这几种语言的绑定模板文件。

----

#### 其他   

*所有导出的函数都为标准的c方式。* 在Windows上采用`__stdcall`约定，其它平台采用`__cdecl`约定。

注：liblcl导出的某些API上用了些看起来怪异的方式，原本liblcl是为govcl所写，所以因为go天生的一些的原因，造成本自己都看着有点不太爽，但又很无奈。

----

##### 字符编码  

在所有平台上都默认使用`utf-8`编码。

----

##### 默认的实例类

*无需手动调用创建和释放。*  

```c
// 定义
TApplication Application; // 应用程序
TScreen Screen;           // 屏幕
TMouse  Mouse;            // 鼠标
TClipboard  Clipboard;    // 剪切板
TPrinter Printer;         // 打印机

// 获取实例类指针
Application = Application_Instance();
Screen = Screen_Instance();
Mouse = Mouse_Instance();              
Clipboard = Clipboard_Instance();      
Printer = Printer_Instance();          
```

----

##### 事件回调

*事件回调分为3种类型。*   

获取参数数组中每个成员  

```c
// x86: sizeof(uintptr_t) = 4
// x64: sizeof(uintptr_t) = 8

// 从指定索引和地址获取事件中的参数
#define getParamOf(index, ptr) \
 (*((uintptr_t*)((uintptr_t)ptr + (uintptr_t)index*sizeof(uintptr_t))))
```


* 基本事件回调

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


// 回调函数原型
// f:        通过SetOnXXX传入的Id或者函数指针
// args:     参数组数指针，通过getParamOf来获取每个成员
// argcount: 参数数组长度
// 事件回调
static void* LCLAPI doEventCallbackProc(void* f, void* args, long argCount) {
 
	// 获取参数的宏  
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
    return NULL;
}

// 设置回调
SetEventCallback(GET_CALLBACK(doEventCallbackProc));
```

* TForm消息回调  
```c
// f: addr
// msg: TMessage
void* LCLAPI doMessageCallbackProc(void* f, void* msg) {
   ((void(*)(void*))f)(msg);
    return NULL;
}

// 设置回调
SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
```

* 线程同步回调  
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
SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));

// 线程同步操作
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

##### 集合类型操作

```c
// 集合加法，val...中存储为位的索引，下标为0
TSet Include(TSet s, uint8_t val) {
    return (TSet)(s | (1 << val));
}

// 集合减法，val...中存储为位的索引，下标为0
TSet Exclude(TSet s, uint8_t val) {
    return (TSet)(s & (~(1 << val)));
}

// 集合类型的判断，val表示位数，下标为0
BOOL InSet(uint32_t s, uint8_t val) {
    if ((s&(1 << val)) != 0) {
        return TRUE;
    }
    return FALSE;
}
```

----

##### 初始liblcl示例

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
    SetEventCallback(GET_CALLBACK(doEventCallbackProc));
    // 设置消息回调
    SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
    // 设置线程同步回调
    SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));
    // 初始实例类
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

### c语言调用liblcl示例

```c

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
    // GetFPStringArrayMember 为一个从Lazarus的string数组中获取成员的函数。
    for (i = 0; i < len; i++) {
        
#ifdef _WIN32
        // 由于liblcl使用的是UTF-8编码，所以获取或者传入的在Windows下都要经过UTF-8编/解码 
        char *filename = UTF8Decode(GetFPStringArrayMember(aFileNames, i));
#else
        // Linux与macOS默认都是UTF-8，则无需编/解码
        char *filename = GetFPStringArrayMember(aFileNames, i);
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
----

### 授权

**保持跟Lazarus LCL组件采用相同的授权协议: [COPYING.modifiedLGPL](COPYING.modifiedLGPL.txt)**  