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

##### 使用结构化异常处理的函数（dll中导出）

##### Use structured exception handling functions (exported in dll)  

```c
// 类型定义(Type definition)
typedef uint64_t(LCLAPI *MYSYSCALL)(void*, intptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);  

// 从DLL中获取此函数(Get this function from DLL)
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
// 定义(definition)
static TApplication Application; // 应用程序(Application)
static TScreen Screen;           // 屏幕(Screen)
static TMouse  Mouse;            // 鼠标(Mouse)
static TClipboard  Clipboard;    // 剪切板(Clipboard)
static TPrinter Printer;         // 打印机(Printer)  

// 获取实例类指针(Get instance class pointer)
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

// 从指定索引和地址获取事件中的参数(Get the parameters in the event from the specified index and address)
#define getParamOf(index, ptr) \
 (*((uintptr_t*)((uintptr_t)ptr + (uintptr_t)index*sizeof(uintptr_t))))
```



* 基本事件回调(Basic event callback)  

```c
#define GET_VAL(index) \
getParamOf(index, args)

#define Syscall12(addr, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) \
    ((void(*)(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t))addr)(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)


// 回调函数原型(Callback function prototype)
// f: 通过SetOnXXX传入的Id或者函数指针(Id or function pointer passed in through SetOnXXX) 
// args: 参数组数指针，通过getParamOf来获取每个成员(Parameter group number pointer, Get each member by getParamOf)
// argcount: 参数数组长度(Parameter array length)
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
    return NULL; // 总是返回NULL即可(Always return NULL)
}
// 设置回调(Set callback)
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

// 设置回调(Set callback)
SetMessageCallback(GET_CALLBACK(doMessageCallbackProc));
```

* 线程同步回调(Thread synchronization callback)  
```c

TThreadProc threadSyncProc;

void* LCLAPI doThreadSyncCallbackProc() {
    if (threadSyncProc) {
        ((TThreadProc)threadSyncProc)();
        threadSyncProc = NULL;
    }
    return NULL;
}

// 设置回调(Set callback)
SetThreadSyncCallback(GET_CALLBACK(doThreadSyncCallbackProc));

// 
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