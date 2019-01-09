int main()
{
    int a = 0;
    {
        a = 1;
        {
            a = 2;
        }
    }
    return a;
}