from django.http import HttpResponse
from django.shortcuts import render
import redis
from pykafka import KafkaClient

pool = redis.ConnectionPool(host='localhost', port=6379, decode_responses=True,db=4)
pool1 = redis.ConnectionPool(host='localhost', port=6379, decode_responses=True,db=6)
pool2 = redis.ConnectionPool(host='localhost', port=6379, decode_responses=True,db=8)
r = redis.Redis(connection_pool=pool)  # word search times
r1 = redis.Redis(connection_pool=pool1)  # word meanning
r2 = redis.Redis(connection_pool=pool2)  # user_search
client = KafkaClient(hosts="192.168.1.1:9092, 192.168.1.2:9092") # 可接受多个Client这是重点
client.topics  # 查看所有topic
topic = client.topics['user_record'] # 选择一个topic
producer = topic.get_producer()
producer.produce(['test message ' + str(i ** 2) for i in range(4)]) # 加了个str官方的例子py2.7跑不过

def index(request):
    # word = "test"
    # print(r1.get(word))
    return render(request, 'index.html')


def ajax(request):
    word = request.GET.get("word")
    start = '['+word
    end = word[:-1] + 'z'
    ans = {}
    remain = 0 if request.session['user_id'] in request.session.keys() else 2
    wordsearch = r.zrangebylex(name="WordSearch", min=start, max=end)
    if request.session['user_id'] in request.session.keys():
        global ans
        user_id = "#"+request.session['user_id']
        user_map_count = {}
        for each in wordsearch:
            score = r2.zscore(user_id, each)
            user_map_count[each] = score
        ranked = sorted(user_map_count, key=lambda arr: arr[1], reverse=True)
        ans = ranked[0:2]
    # non_register_user
    search_map_count = {}
    for each in wordsearch:
        score = r.zscore('WordCount', each)
        search_map_count[each] = score
    ranked = sorted(search_map_count.items(), key=lambda arr: arr[1], reverse=True)

    while len(ans) < 5:
        ans[ranked[remain][0]] = ranked[remain][1]  # key and value
        remain += 1

    return HttpResponse("Hello world ! ")


# search answer
def reqajax(request):
    has_user = True if request.session['user_id'] in request.session.keys() else False
    word = request.GET.get("word")
    chinese = r1.get(word)

    if has_user:
        r2.zincrby('#'+request.session['user_id'], word, 1)
    r.zincrby('WordCount', word, 1)
    #print(r1.get("hello"))

    return HttpResponse("Hello world !")


def register(request):
    if 'user_id' in request.POST and 'user_password'in request.POST and 'check_password'in request.POST:
        user_id = request.POST['user_id']
        user_passwd = request.POST['user_password']
        user_confirm_passwd = request.POST['check_password']
        if len(user_id) == 0:
            return HttpResponse('账号不能为空')
        elif user_passwd != user_confirm_passwd:
            return HttpResponse("两次密码不一致")
        elif len(user_passwd) == 0 or len(user_confirm_passwd) == 0:
            return HttpResponse('密码不能为空')
        else:
            request.session['user_id'] = user_registed._id
            login_msg = '欢迎:' + request.session['user_id']
    return HttpResponse("登陆成功",user_id)



def login(request):
    global login_msg

    if 'username' in request.POST and 'password' in request.POST:
        username = request.POST['username']
        password = request.POST['password']
        if len(username) == 0 or len(password) == 0:
            return HttpResponse("账号/密码不能为空")
        try:
            #user_logined = Customer.objects(_id=request.POST["username"])[0]
        except:
            return HttpResponse("该账号不存在")
        if password == user_logined.password:
            request.session['user_id'] = user_logined._id        #  从session中获取的id永远是本次登录者的id。
            # login_msg = '欢迎:' + request.session['user_id']
            return render(request, 'index.html', {'login_msg': '欢迎:' + request.session['user_id']})
        else:
            return HttpResponse("密码错误")
    else:
        return HttpResponse("")


def logout(request):
    if 'user_id' in request.session.keys():
        del request.session['user_id']
        return('注销成功')
    else:
        return HttpResponse("请先登录!")