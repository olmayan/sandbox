#include "field.h"
#include <QString>
#include <stdlib.h>

Field::Field(int width, int height)
{
    w = width;
    h = height;
    c = 7;
    pField = new int[w * h];
    memset(pField, 0, w * h * sizeof(int));
}

Field::Field(Field *field)
{
    w = field->w;
    h = field->h;
    pField = new int[w * h];
    memcpy(pField, field->pField, w * h * sizeof(int));
}

Field::~Field()
{
    delete pField;
}

int Field::width()
{
    return w;
}

int Field::height()
{
    return h;
}

int Field::cell(int x, int y)
{
    return pField[x + y * w];
}

void Field::setCell(int x, int y, int value)
{
    pField[x + y * w] = value;
}

int Field::colors()
{
    return c;
}

bool Field::throwBalls(Field *throwMap, int nBalls)
{
    Field clone(this);
    int i, m, n = w * h;
    quint16 *points = new quint16[n];
    bool res = true;

    while (nBalls--)
    {
        m = 0;
        for (i = 0; i < n; i++)
        {
            if (!clone.pField[i])
            {
                points[m] = i;
                m++;
            }
        }
        if (!m) { res = false; break; }
        i = points[qrand() % m];
        throwMap->pField[i] = clone.pField[i] = 1 + qrand() % c;
    }

    delete points;

    return res;
}

void Field::clear()
{
    memset(pField, 0, w * h);
}

bool Field::hasPath(int x1, int y1, int x2, int y2, Field *searchResult)
{
    int step = searchResult->pField[x1 + w * y1] = -1;
    int goal = searchResult->pField[x2 + w * y2] = -w * h;

    bool bContinue;
    int x, y;

    do
    {
        bContinue = false;
        for (y = 0; y < h; y++)
        {
            for (x = 0; x < w; x++)
            {
                if (searchResult->cell(x, y) == step)
                {
                    if (x > 0)
                    {
                        if (searchResult->cell(x - 1, y) == 0)
                        {
                            searchResult->setCell(x - 1, y, step - 1);
                            bContinue = true;
                        }
                        else if (searchResult->cell(x - 1, y) == goal)
                            return true;
                    }
                    if (x < w - 1)
                    {
                        if (searchResult->cell(x + 1, y) == 0)
                        {
                            searchResult->setCell(x + 1, y, step - 1);
                            bContinue = true;
                        }
                        else if (searchResult->cell(x + 1, y) == goal)
                            return true;
                    }
                    if (y > 0)
                    {
                        if (searchResult->cell(x, y - 1) == 0)
                        {
                            searchResult->setCell(x, y - 1, step - 1);
                            bContinue = true;
                        }
                        else if (searchResult->cell(x, y - 1) == goal)
                            return true;
                    }
                    if (y < h - 1)
                    {
                        if (searchResult->cell(x, y + 1) == 0)
                        {
                            searchResult->setCell(x, y + 1, step - 1);
                            bContinue = true;
                        }
                        else if (searchResult->cell(x, y + 1) == goal)
                            return true;
                    }
                }
            }
        }
        step--;
    } while (bContinue);

    return false;
}
